#' Group overhead modules into 1 module
#'
#' @details Solves the problem where incidents had too many unique overhead
#' modules by changing the all overhead modules to "O-100".
#'
#' @param mod_ids character vector of mod_ids
clean_mods <- function(mod_ids) {
  overhead <- grepl("^O-\\d", as.vector(mod_ids))
  mod_ids[overhead] <- "O-100"

  as.character(mod_ids)
}

#' Assign roles to agents
#' @param input_df data frame containing agent data
#' @param max_leads number of leads per module
#' @param prop_overhead_leads proportion of overhead to assign as leads
assign_roles <- function(input_df, max_leads, prop_overhead_leads) {
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
          max_leads <- round(num_in_mod * prop_overhead_leads)
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
#' Find out which agents to vaccinate and assign immunity
#' @param input_df data frame containing agent roster
#' @param vax_efficacy proportion of vaccinated agents the gain immunity
#' @param vax_df 1 row data frame, colnames are res_gaccs, row values are the total number of vaccinated agents after vaccination for that time step. if the vax_df cell for gacc NM-SWC is 100 and 50 agents are already vaccinated, this function will return 50 ids to vaccinate
#' @returns nested list of res_ids; output$vaccinated and output$immune
vaccinate <- function(input_df, vax_efficacy = 0.95, vax_df = NULL) {
  splitby_gacc <- split(input_df, input_df$res_gacc)

  vax_ids <- lapply(
    splitby_gacc,
    function(gacc_df) {
      rate <- gacc_df$vax_rate[1]
      if (is.null(vax_df)) {
        num_to_vax <- round(nrow(gacc_df) * rate)
      } else {
        num_vaccinated <- length(which(gacc_df$vaccinated))
        home_gacc <- gacc_df$res_gacc[1]
        num_to_vax <- vax_df[1, home_gacc] - num_vaccinated
      }
      candidates <- gacc_df$res_id[!gacc_df$vaccinated &
                                     !gacc_df$quarantine &
                                     gacc_df$inc_id == 0]

      if (length(candidates) < num_to_vax) {
        candidates
      } else {
        sample(candidates, num_to_vax)
      }
    }
  )
  vax_ids <- as.numeric(unlist(vax_ids))
  # This method is deterministic, it guarantees that the proportion of
  # immune agents is equal to the vax_efficacy
  immune_ids <- sample(vax_ids, round(length(vax_ids) * vax_efficacy))

  return(list(vaccinated=vax_ids, immune=immune_ids))
}

mk_agents <- function(inc_data, mod_data, day) {
  total <- nrow(inc_data)

  data.frame(
    res_id = inc_data[["res_id"]],
    res_gacc = inc_data[["res_gacc"]],
    inc_id = inc_data[[day + 2]], # first 2 columns are res_id & res_gacc
    mod_id = mod_data[[day + 2]],
    leader = vector("logical", total),
    state = "S",
    quarantine = vector("logical", total),
    q_days = 0
    )
}
