# exposure functions

expose_modules <- function(input_df, BA, BI, exp_thres = 0, delta_t) {
  splitby_mod <- split(input_df, paste0(input_df$inc_id, "_", input_df$mod_id))

  out <- lapply(
    splitby_mod,
    function(mod_df) {
      if (mod_df$inc_id[1] > 0) {
        non_q <- length(which(!mod_df$quarantine))
        I <- length(which(mod_df$state == "I" & !mod_df$quarantine))
        A <- length(which(mod_df$state == "A" & !mod_df$quarantine))

        if (I+A > 0) {
          avg_inf_contacts <- (BI*I + BA*A) * delta_t / non_q
          vnum_inf_contacted <- stats::rpois(nrow(mod_df), avg_inf_contacts)
          mod_df$res_id[which(mod_df$state == "S" &
                                !mod_df$quarantine &
                                vnum_inf_contacted > exp_thres)]
        }
      }
    }
  )
  as.numeric(unlist(out))
}

#' Exposes agents based on assigned roles
#' @param input_df Single incident data frame. The template for calculating the new data frame
#' @param BI Beta I
#' @param BA Beta A
#' @param exp_thres How many exposures before infection, default is 1.
#' @param delta_t Time step

expose_leads <- function(input_df, BI, BA, exp_thres = 0, delta_t) {
  splitby_inc <- split(input_df, input_df$inc_id)

  out <- lapply(
    splitby_inc,
    function(inc_df) {
      if (inc_df$inc_id[1] > 0) {
        non_q <- length(which(inc_df$leader & !inc_df$quarantine))
        I <- length(which(inc_df$leader & inc_df$state == "I"))
        A <- length(which(inc_df$leader & inc_df$state == "A"))
        if (I+A > 0) {
          avg_inf_contacted <- (BI*I + BA*A) * delta_t / non_q
          vnum_inf_contacted <- stats::rpois(nrow(inc_df), avg_inf_contacted)
          inc_df$res_id[which(inc_df$state == "S" &
                                inc_df$leader &
                                !inc_df$quarantine &
                                vnum_inf_contacted > exp_thres)]
        }
      }
    }
  )
  as.numeric(unlist(out))
}
