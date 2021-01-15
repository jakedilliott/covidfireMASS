# exposure functions

which_exposed <- function(input_df, BA, BI, exp_thres = 1, delta_t) {
  split_by_mod <- unique_mod_split(input_df)

  out <- purrr::map(
    split_by_mod,
    function(module) {
      if (module$inc_id[1] > 0){
        N <- length(which(module$q_status == 0))
        I <- length(which(module$state == 2 & module$q_status == 0))
        A <- length(which(module$state == 4 & module$q_status == 0))
        if (N > 1) {cat(paste0(module$inc_id[1], module$mod_id[1], "\n"))}
        if (I + A > 0) {
          cat(paste0(module$inc_id[1], module$mod_id[1], "N:", N, "/ I:", I, "/ A:", A, "\n"))
          avg_inf_contacted <- (BI*I + BA*A) * delta_t / N
          vnum_inf_contacted <- stats::rpois(nrow(module), avg_inf_contacted)

          print(module$res_id[which(module$state == 0 &
                                      module$module$q_status == 0 &
                                      vnum_inf_contacted > exp_thres)])
        }
      }
    }
  )
  unlist(out, recursive = FALSE)
}

#' Exposes agents based on assigned modules
#' @param inc_df Single incident data frame. The template for calculating the new data frame.
#' @param new_df The data frame to be modified
#' @param BI Beta I
#' @param BA Beta A
#' @param exp_thres How many exposures before infection, default is 1.
#' @param delta_t Time step
expose_modules <- function(inc_df, new_df, BA, BI, exp_thres, delta_t) {
  if (inc_df[["inc_id"]][1] > 0) {
    inc_split <- split(inc_df, inc_df[["mod_id"]])
    new_split <- split(new_df, new_df[["mod_id"]])

    new_df <- purrr::map2_dfr(
      inc_split, new_split,
      function(module, new_module) {
        I <- length(which(module[["q_status"]] == 0 && module[["state"]] == 2))
        A <- length(which(module[["q_status"]] == 0 && module[["state"]] == 4))
        N <- length(which(module[["q_status"]] == 0))
        avg_inf_contacted <- (BI * I + BA * A) * delta_t / N
        vnum_inf_contacted <- stats::rpois(nrow(module), avg_inf_contacted)

        new_module[["state"]][which(module[["q_status"]] == 0 && module[["state"]] == 0 &&
          vnum_inf_contacted > exp_thres)] <- 1

        new_module # purr function output
      }
    )
    return(new_df)
  } else {
    return(inc_df)
  }
}

#' Exposes agents based on assigned roles
#' @param inc_df Single incident data frame. The template for calculating the new data frame.
#' @param new_df The data frame to be modified
#' @param BI Beta I
#' @param BA Beta A
#' @param exp_thres How many exposures before infection, default is 1.
#' @param delta_t Time step

expose_leads <- function(inc_df, new_df, BI, BA, exp_thres, delta_t) {
  I <- length(which(inc_df[["role"]] == 1 && inc_df[["q_status"]] == 0 && inc_df[["state"]] == 2))
  A <- length(which(inc_df[["role"]] == 1 && inc_df[["q_status"]] == 0 && inc_df[["state"]] == 4))
  N <- length(which(inc_df[["role"]] == 1 && inc_df[["q_status"]] == 0))
  avg_inf_contacted <- (BI * I + BA * A) * delta_t / N
  vnum_inf_contacted <- stats::rpois(nrow(inc_df), avg_inf_contacted)

  new_df[["state"]][which(inc_df[["role"]] == 1 && inc_df[["q_status"]] == 0 &&
    inc_df[["state"]] == 0 && vnum_inf_contacted > exp_thres)] <- 1

  new_df
}
