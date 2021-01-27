# R/seasonal_model.R

#' Seasonal agent-based model simulation
#' @param inc_data Resource assignments for incident IDs
#' @param mod_data Resource assignments for module IDs
#' @param inc_info Additional information on each incident
#' @param incl_A Include Asymptomatic, TRUE or FALSE
#' @param De Incubation period
#' @param gamma Recovery time in days
#' @param eir Entry infection rate
#' @param R_init Initial number of recovered agents
#' @param R0 Basic reproduction parameter
#' @param max_leads Number of desired leads per module
#' @param pIR Probability that Symptomatic agents do not quarantine
#' @param pAQ Probability that Asymptomatic will be caught and quarantined
#' @param I_init Parameters for setting initial infective agents
#' @export
seasonal_sim <- function(inc_data, mod_data, inc_info, incl_A = TRUE, De = 5,
                         gamma = 8, eir = 0.005, R_init = 0, R0 = 1.4, max_leads = 2,
                         pIR = 0.2, pAQ = 0.3, I_init = 0) {
  # Get data in order ----
  # inc_data <- inputs$incidents
  # mod_data <- inputs$modules

  set.seed(123)

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

  BA <- ifelse(incl_A == TRUE, R0int*R0 / ( (z*pI/((pIR/DiI) + ((1-pIR)/DiAI))) + ((1-pI)/((pAQ/DiAQ) + ((1-pAQ)/DiAI))) ), 0)
  BI <- ifelse(incl_A == TRUE, R0int*BA*z, R0/DiI)

  agent_df <- mk_agents(inc_data, mod_data, 1)
  agent_df$res_gacc <- clean_gacc(inc_data, inc_info)
  agent_df$mod_id <- clean_mods(agent_df$mod_id)
  agent_df$leader[agent_df$res_id %in% assign_roles(agent_df, max_leads)] <- TRUE

  if (I_init > 0) {
    agent_df$state[sample(N, I_init)] <- 2
  }

  outputs <- list() # create outputs list
  while (t < tend) {
    # recording outputs
    outputs[[t]] <- dplyr::mutate(agent_df, time = t)

    # Mobs and Demobs
    new_df <- agent_df
    new_df$inc_id <- inc_data[[t + 2]]
    new_df$mod_id <- mod_data[[t + 2]]

    new_df$mod_id <- clean_mods(new_df$mod_id)
    new_df$leader[new_df$inc_id != agent_df$inc_id] <- FALSE
    new_df$leader[new_df$res_id %in% assign_roles(new_df, max_leads)] <- TRUE

    # Exposure operations ----
    exposed_res_ids <- append(
      expose_modules(agent_df, BA, BI, exp_thres, delta_t),
      expose_leads(agent_df, BI, BA, exp_thres, delta_t)
    )
    # print(exposed_res_ids)
    if (!is.null(exposed_res_ids)) {
      new_df$state[which(new_df$res_id %in% exposed_res_ids)] <- "E"
    }

    # state changes ----
    # random rolls
    rE <- stats::runif(N) # draw to become exposed
    rI <- stats::runif(N) # draw to become infective
    rS <- stats::runif(N) # draw to become symptomatic
    rQ <- stats::runif(N) # getting caught and moving to quarantine
    rR <- stats::runif(N) # draw to recover
    pRecover   <- 1 - exp(-1/gamma * delta_t) # p of recovery
    pInfective <- 1 - exp(-1/De * delta_t) # p of becoming infective

    # Agents that have changed incidents can be infected during mobilization
    new_df$state[which(agent_df$inc_id != new_df$inc_id &
                         agent_df$state == "S" &
                         rE < eir)] <- "E"

    # After incubation period Exposed move to Infected or Asymptomatic
    new_df$state[which(agent_df$state == "E" & rI < pInfective & rS < pI)] <- "I"
    new_df$state[which(agent_df$state == "E" & rI < pInfective & rS > pI)] <- "A"

    # Infected(Symptomatic) recognize symptoms and quarantine or don't catch
    # symptoms and recover
    new_df$state[which(agent_df$state == "I" & rR < pRecover)] <- "R"
    new_df$quarantine[which(agent_df$state == "I" & rQ < 1-pIR)] <- TRUE

    # Asymptomatic are caught by testing/screening and quarantine or they are
    # not caught and recover
    new_df$state[which(agent_df$state == "A" & rR < pRecover)] <- "R"
    new_df$quarantine[which(agent_df$state == "A" & rQ < pAQ)] <- TRUE

    # module quarantine
    res_ids_to_q <- modular_quarantine(agent_df, pIR, pAQ)
    if (!is.null(res_ids_to_q) & length(res_ids_to_q) > 0) {
      new_df$quarantine[which(agent_df$res_id %in% res_ids_to_q)] <- TRUE
    }

    # Quarantine operations
    new_df$days_q[which(agent_df$quarantine)] %+=% 1 # increment q_days

    # Agents leave isolation only if they have been isolated a minimum of days
    # and they are Susceptible or Recovered
    new_df$q_days[which(agent_df$state %in% c("S", "R") &
                          agent_df$quarantine &
                          agent_df$q_days >= Dq)] <- 0
    new_df$quarantine[which(agent_df$state %in% c("S", "R") &
                              agent_df$quarantine &
                              agent_df$q_days >= Dq)] <- FALSE

    # clean up
    agent_df <- new_df
    t <- t + delta_t
  }

  dplyr::bind_rows(outputs)
}
