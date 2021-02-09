# On Fire Functions =====

#' Find exposed agents by unique module (inc_id x mod_id)
#' @param input_df Input data frame
#' @param BA Beta for Asymptomatic
#' @param BI Beta for Symptomatic
#' @param exp_thres Exposure threshold
#' @param delta_t Time step
#' @returns Vector of resource ID's that have been newly exposed
expose_modules <- function(input_df, BA, BI, exp_thres = 0, delta_t = 1) {
  splitby_mod <- split(input_df, paste0(input_df$inc_id, "_", input_df$mod_id))

  out <- lapply(
    splitby_mod,
    function(mod_df) {
      if (mod_df$inc_id[1] > 0) {
        non_q <- length(which(!mod_df$quarantine))
        I <- length(which(mod_df$state == "I" & !mod_df$quarantine))
        A <- length(which(mod_df$state == "A" & !mod_df$quarantine))

        if (I+A > 0 & non_q > 0) {
          avg_inf_contacts <- (BI*I + BA*A) * delta_t / non_q
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
  )
  as.numeric(unlist(out))
}

#' Find exposed leaders by unique fire (inc_id)
#' @param input_df Input data frame
#' @param BI Beta for Symptomatic
#' @param BA Beta for Asymptomatic
#' @param exp_thres Exposure threshold
#' @param delta_t Time step
#' @returns Vector of resource ID's that have been newly exposed
expose_leads <- function(input_df, BI, BA, exp_thres = 0, delta_t = 1) {
  splitby_inc <- split(input_df, input_df$inc_id)

  out <- lapply(
    splitby_inc,
    function(inc_df) {
      if (inc_df$inc_id[1] > 0) {
        non_q <- length(which(inc_df$leader & !inc_df$quarantine))
        I <- length(which(inc_df$leader & inc_df$state == "I"))
        A <- length(which(inc_df$leader & inc_df$state == "A"))
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
  )
  as.numeric(unlist(out))
}

# Off Fire Functions =====

#' Update the default EIR list
#' @param eir_ls named list containing infection rates (list(gacc = rate))
#' @param custom_rates Named list containing new infection rates to be applied
update_eir <- function(input_df, custom_rates, eir) {
  sapply(input_df$res_gacc, function(x) {
    if(x %in% names(custom_rates)) {
      custom_rates[[x]]
    }
    else {
      eir
    }
  })
}

#' Find exposed agents that are off fire
#' @param input_df complete agent data frame (all incidents and modules)
expose_off_fire <- function(input_df) {
  off_fire <- input_df[input_df$inc_id == 0, ]
  rE <- runif(nrow(off_fire))

  off_fire$res_id[off_fire$state == "S" &
                    !off_fire$quarantine &
                    rE < off_fire$gacc_eir]
}

