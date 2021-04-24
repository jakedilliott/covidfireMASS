# R/seasonal_model.R

#' Seasonal agent-based model simulation
#' @param inc_data Resource assignments for incident IDs
#' @param mod_data Resource assignments for module IDs
#' @param inc_info Additional information on each incident
#' @param overhead_ids Which res id's should be considered seasonal overhead (for vaccination purposes)
#' @param incl_A Include Asymptomatic, TRUE or FALSE
#' @param De Incubation period
#' @param gamma Recovery time in days
#' @param eir Entry infection rate
# @param custom_eir Named list of customized GACC infection rates
#' @param R_init Initial number of recovered agents
#' @param I_init Initial number of infectious agents
#' @param vax_init Initial number of vaccinated agents
# @param vax_rate Percent of total population to vaccinate at each time step
# @param varying_vax Named list specifying a gacc and its new vax rate, for example list(gacc = "NM-SWC", rate = 0.2)
#' @param vax_efficacy Proportion of vaccinated agents that become immune
#' @param vax_df Data frame with specified number of agents to vaccinate at each time step
# @param R0 Basic reproduction parameter
#' @param BI Beta for symptomatic agents
#' @param module_multiplier Scales up the infectiousness of agents within crew and equipment modules
#' @param max_leads Number of desired leads per module
#' @param prop_overhead_leads Proportion of the overhead module that should be leaders
#' @param pIQ Probability that Symptomatic agents do not quarantine
#' @param pAQ Probability that Asymptomatic will be caught and quarantined
#' @param .raw If true save all agent data at each timestep, default is FALSE
#'
#' @importFrom dplyr count
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_rows
#' @importFrom stats runif
#'
#' @export
seasonal_sim <- function(inc_data,
                         mod_data,
                         inc_info,
                         overhead_ids = NULL,
                         incl_A = TRUE,
                         De = 5,
                         gamma = 8,
                         eir = 0.005,
                         # custom_eir = "none",
                         R_init = 0,
                         I_init = 0,
                         vax_init = 0,
                         # vax_rate = 0,
                         # varying_vax = NULL,
                         vax_efficacy = 0.95,
                         vax_df = NULL,
                         # R0 = 1.4,
                         BI = 0.15,
                         module_multiplier = 4,
                         max_leads = 2,
                         prop_overhead_leads = 0.5,
                         pIQ = 0.5,
                         pAQ = 0,
                         .raw = FALSE) {

  # set.seed(123)

  # Parameters ----
  N <- nrow(inc_data)
  t <- 1
  delta_t <- 1
  tend <- ncol(inc_data) - 2 # first 2 cols are res_id and res_gacc

  exp_thres <- 0
  DiI <- 8-De
  DiAI <- 8
  Dq <- 10
  DiAQ <- 3
  z <- 1.65
  R0int <- 1
  pI <- 0.4286
  BA <- (2/3) * BI
  pIQ <- 1 - ((1-pIQ)^(1/7))


  # BA <- ifelse(incl_A == TRUE, R0int*R0 / ( (z*pI/((pIR/DiI) + ((1-pIR)/DiAI))) + ((1-pI)/((pAQ/DiAQ) + ((1-pAQ)/DiAI))) ), 0)
  # BI <- ifelse(incl_A == TRUE, R0int*BA*z, R0/DiI)

  # Pre-processing / clean up of inc_id and mod_id data frame
  mod_data <- clean_mods(mod_data)
  inc_data$res_gacc <- clean_gacc(inc_data, inc_info)

  # Setting up the agent dataframe
  agent_df <- agents_init(inc_data, mod_data, inc_info, 1,
                          nleads=max_leads, p_overhead_leads=prop_overhead_leads,
                          R_init=R_init, I_init=I_init, vax_init=vax_init,
                          vax_efficacy=vax_efficacy)

  # if (!is.null(varying_vax)){
  #   agent_df$vax_rate[agent_df$res_gacc %in% varying_vax$gacc] <- varying_vax$rate
  # }

  outputs <- list() # create outputs list
  while (t < tend) {
    # recording outputs
    agent_df$time <- t
    if (.raw) {
      outputs[[t]] <- agent_df
    } else {
      summarise_t <- count(agent_df, all_of("time", "inc_id", "state", "quarantine", "vaccinated"))
      outputs[[t]] <- summarise_t
    }

    # Mobs and Demobs
    new_df <- agent_df
    new_df$inc_id <- inc_data[[t + 2]]
    new_df$mod_id <- mod_data[[t + 2]]

    # new_df$mod_id <- clean_mods(new_df$mod_id) # moved to pre-processing
    new_df$leader[new_df$inc_id != agent_df$inc_id] <- FALSE
    new_df$leader[new_df$res_id %in% assign_roles(new_df, max_leads, prop_overhead_leads)] <- TRUE

    # Exposure operations ----
    exposed_res_ids <- c(
      expose_modules(agent_df, BA, BI, module_multiplier, exp_thres, delta_t),
      expose_leads(agent_df, BI, BA, exp_thres, delta_t),
      expose_off_fire(agent_df, eir)
    )

    if (!is.null(exposed_res_ids)) {
      new_df$state[which(new_df$res_id %in% exposed_res_ids)] <- "E"
    }

    # state changes ----
    # random rolls
    rE <- runif(N) # draw to become exposed
    rI <- runif(N) # draw to become Infectious
    rS <- runif(N) # draw to become symptomatic
    rQ <- runif(N) # getting caught and moving to quarantine
    rR <- runif(N) # draw to recover
    rVax <- runif(N)

    pRecover   <- 1 - exp(-(1/gamma) * delta_t) # p of recovery
    pInfectious <- 1 - exp(-(1/De) * delta_t) # p of becoming Infectious

    # After incubation period Exposed move to Infected or Asymptomatic
    new_df$state[which(agent_df$state == "E" & rI < pInfectious & rS < pI)] <- "I"
    new_df$state[which(agent_df$state == "E" & rI < pInfectious & rS > pI)] <- "A"

    # Infected(Symptomatic) recognize symptoms and quarantine or don't catch
    # symptoms and recover
    new_df$state[which(agent_df$state == "I" & rR < pRecover)] <- "R"
    # new_df$quarantine[which(agent_df$state == "I" & rQ < pIQ)] <- TRUE

    # Asymptomatic are caught by testing/screening and quarantine or they are
    # not caught and recover
    new_df$state[which(agent_df$state == "A" & rR < pRecover)] <- "R"
    # new_df$quarantine[which(agent_df$state == "A" & rQ < pAQ)] <- TRUE

    # module quarantine
    res_ids_to_q <- modular_quarantine(agent_df, pIQ, pAQ)
    if (!is.null(res_ids_to_q) & length(res_ids_to_q) > 0) {
      new_df$quarantine[which(agent_df$res_id %in% res_ids_to_q)] <- TRUE
    }

    # Quarantine operations
    new_df$days_q[which(agent_df$quarantine)] %+=% 1 # increment q_days

    # Agents leave isolation only if they have been isolated a minimum of days
    # and they are Susceptible or Recovered
    leaving_quarantine <- agent_df$res_id[agent_df$state %in% c("S", "R") &
                                            agent_df$quarantine &
                                            agent_df$q_days >= Dq]
    new_df$q_days[new_df$res_id %in% leaving_quarantine] <- 0
    new_df$quarantine[new_df$res_id %in% leaving_quarantine] <- FALSE

    # Vaccination
    if (is.null(vax_df)) {
      stop("vax_df is missing")
    } else {
      vaccinated_agents <- vaccinate(agent_df,
                                     overhead_ids,
                                     method = vax_df$method,
                                     plan = vax_df$plan[t, ],
                                     efficacy = vax_efficacy)
    }
    new_df$vaccinated[new_df$res_id %in% vaccinated_agents$vaccinated] <- TRUE
    new_df$state [new_df$res_id %in% vaccinated_agents$immune] <- "R"

    # clean up
    agent_df <- new_df
    t <- t + delta_t
  }

  bind_rows(outputs)
}
