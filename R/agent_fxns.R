#' Initialize agent data frame for simulation input
#'
#' @param inc_assignments daily incident assignment data
#' @param mod_assignments daily module assignment data
#' @param inc_info incident information for a whole season
#' @param time which day of the season should the agents start on
#' @param nleads number of leads each module should have
#' @param p_overhead_leads proportion of overhead modules that should be leaders
#' @param R_init proportion of the population that has already recovered
#' @param I_init proportion of the population that enters the season infected
#' @param vax_init proportion of the population that enters the season vaccinated
#' @param vax_efficacy what proportion of those vaccinated actually become immune
#'
#' @return Agent data frame
#' @export
agents_init <- function(inc_assignments, mod_assignments, inc_info, time,
                        nleads, p_overhead_leads,
                        R_init=0, I_init=0, vax_init=0, vax_efficacy=0.95) {
  n <- nrow(inc_assignments)

  df <- tibble::tibble(
    res_id = inc_assignments[["res_id"]],
    res_gacc = clean_gacc(inc_assignments, inc_info),
    inc_id = inc_assignments[[time + 2]], # first 2 columns are res_id & res_gacc
    mod_id = mod_assignments[[time + 2]],
    leader = vector("logical", n),
    state = "S",
    quarantine = vector("logical", n),
    q_days = 0,
    vaccinated = vector("logical", n),
    time   = time
  )

  # assign leads
  df$leader[df$res_id %in% assign_roles(df, nleads, p_overhead_leads)] <- TRUE

  # initial recovered
  if (R_init > 0) {
    recovered <- sample(n, R_init * n)
    df$state[recovered] <- "R"
  }
  #initial vaccinated
  if (vax_init > 0) {
    vaccinated <- sample(n, round(vax_init * n))
    immune <- sample(vaccinated, round(vax_efficacy * vax_init)) # deterministic method
    df$vaccinated[vaccinated] <- TRUE
    df$state[immune] <- TRUE
  }
  # initial infectious
  if (I_init > 0) {
    total_infectious <- I_init * n
    I <- round(total_infectious * 0.4286) # proportion symptomatic
    infectious <- sample(which(df$state == "S"), total_infectious)
    df$state[infectious[1:I]] <- "I"
    df$state[infectious[-(1:I)]] <- "A"
  }

  return(df)
}


#' Assign roles to agents
#'
#' @param input_df data frame containing agent data
#' @param max_leads number of leads per module
#' @param p_overhead_leads proportion of overhead to assign as leads
assign_roles <- function(input_df, max_leads, p_overhead_leads) {
  split_df <- split(input_df, paste0(input_df$inc_id, "_", input_df$mod_id))

  out <- lapply(
    split_df,
    function(mod_df) {
      num_in_mod <- nrow(mod_df)
      lead_ids <- mod_df$res_id[mod_df$leader]
      non_lead_ids <- mod_df$res_id[!mod_df$leader]
      num_leads <- length(lead_ids)
      num_non_leads <- length(non_lead_ids)

      # only assign roles to agents on a fire
      if (0 %in% mod_df$inc_id) {
        mod_df$leader <- FALSE
      } else {
        if ("O-100" %in% mod_df$mod_id) {
          max_leads <- round(num_in_mod * p_overhead_leads)
        }
        if (num_in_mod <= max_leads) { # less agents in mod than max_leads
          mod_df$leader <- TRUE
        } else { # more agents in mod than max_leads
          if (num_leads > max_leads) {               # too many leaders, this should always fail
            num_to_demote <- num_leads - max_leads   # because we reset leaders during migration
            ids_to_demote <- sample(lead_ids, num_to_demote)
            mod_df$leader[mod_df$res_id %in% ids_to_demote] <- FALSE
          } else if (num_leads < max_leads) {
            num_to_promote <- max_leads - num_leads
            ids_to_promote <- ifelse(num_non_leads > num_to_promote, # if
                                     sample(non_lead_ids, num_to_promote), # then
                                     non_lead_ids) # else
            mod_df$leader[mod_df$res_id %in% ids_to_promote] <- TRUE
          }
        }
      }
      return(mod_df$res_id[mod_df$leader]) # output for anonymous function
    }
  )
  return(as.numeric(unlist(out))) # final
}


