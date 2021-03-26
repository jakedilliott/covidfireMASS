# On Fire Functions =====

#' Find exposed agents within a single module
#' @param mod_df data frame containing agent information for a single module
#' @param BA Beta for Asymptomatic
#' @param BI Beta for Symptomatic
#' @param multiplier scalar for increasing R0 within modules
#' @param exp_thres Exposure threshold
#' @param delta_t Time step
#' @returns Vector of resource ID's that have been newly exposed'
#' @export
find_exposed_in_mod <- function(mod_df, BA, BI, multiplier, exp_thres = 0, delta_t = 1) {
  if (mod_df$inc_id[1] > 0) {
    non_q <- length(which(!mod_df$quarantine))
    I <- length(which(mod_df$state == "I" & !mod_df$quarantine))
    A <- length(which(mod_df$state == "A" & !mod_df$quarantine))

    if (I+A > 0 & non_q > 0) {
      avg_inf_contacts <- multiplier * (BI*I + BA*A) * delta_t / non_q
      vnum_inf_contacted <- stats::rpois(nrow(mod_df), avg_inf_contacts)
      mod_df$res_id[which(mod_df$state == "S" &
                            !mod_df$quarantine &
                            vnum_inf_contacted > exp_thres)]
    } else {
      0
    }
  } else {
    0
  }
}

#' Find exposed agents by unique module (inc_id x mod_id)
#' @param input_df Input data frame
#' @param ... Exposure params to pass to find_exposed_in_mod; BA, BI, exp_thres, delta_t
#' @returns Vector of resource ID's that have been newly exposed
expose_modules <- function(input_df, ...) {
  split_by_mod <- split(input_df, paste0(input_df$inc_id, "_", input_df$mod_id))
  out <- lapply(split_by_mod, function(x) find_exposed_in_mod(x, ...))

  as.numeric(unlist(out))
}

#' Find exposed leads within a single incident
#' @param inc_df data frame containing agent information for one incident
#' @param BI Beta for Symptomatic
#' @param BA Beta for Asymptomatic
#' @param exp_thres Exposure threshold
#' @param delta_t Time step
#' @returns vector of resource ID's to be exposed
#' @export
find_exposed_in_leads <- function(inc_df, BI, BA, exp_thres = 0, delta_t = 1) {
  if (inc_df$inc_id[1] > 0) {
    non_q <- length(which(inc_df$leader & !inc_df$quarantine))
    I <- length(which(inc_df$leader & inc_df$state == "I" & !inc_df$quarantine))
    A <- length(which(inc_df$leader & inc_df$state == "A" & !inc_df$quarantine))
    if (I+A > 0 & non_q > 0) {
      avg_inf_contacted <- (BI*I + BA*A) * delta_t / non_q
      vnum_inf_contacted <- stats::rpois(nrow(inc_df), avg_inf_contacted)
      inc_df$res_id[which(inc_df$state == "S" &
                            inc_df$leader &
                            !inc_df$quarantine &
                            vnum_inf_contacted > exp_thres)]
    } else {
      0
    }
  } else {
    0
  }
}

#' Find exposed leaders by unique fire (inc_id)
#' @param input_df Input data frame
#' @param ... Exposure params to pass to find_exposed_in_leads
#' @returns Vector of resource ID's that have been newly exposed
expose_leads <- function(input_df, ...) {
  split_by_inc <- split(input_df, input_df$inc_id)
  out <- lapply(split_by_inc, function(x) find_exposed_in_leads(x, ...))

  as.numeric(unlist(out))
}

# Off Fire Functions =====
#' Find exposed agents that are off fire
#' @param input_df complete agent data frame (all incidents and modules)
#' @param eir entry infection rate, or off fire exposure rate
expose_off_fire <- function(input_df, eir) {
  off_fire <- input_df[input_df$inc_id == 0, ]
  rE <- stats::runif(nrow(off_fire))

  off_fire$res_id[off_fire$state == "S" &
                    !off_fire$quarantine &
                    rE < eir]
}
