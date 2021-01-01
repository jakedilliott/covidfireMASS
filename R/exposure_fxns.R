# exposure functions

#' Exposes agents based on assigned modules
#' @param inc_df data frame containing agent data for a single inc
#' @param params list of parameters for infection dynamics

expose_modules <- function(inc_df, params) {
  if (!is.data.frame(inc_df)) {
    stop("Input 'inc_df' is not a data frame")
  }
  if (!is.list(params)) {
    stop("Input 'params' is not a list")
  }
  if (is.null(names(params))) {
    stop("List 'params' must have named elements")
  }
  sapply(c("BA", "BI", "delta_t", "exp_thres"),
         function(x) {
           if (!(x %in% names(params))) {
             stop("Named parameter is missing, check for BA, BI, delta_t, and exp_thres")
           }
         })

  if (inc_df$inc_id[1] == 0) {
    return(inc_df)
  } else {
    mod_dfs <- mod_subsetter(inc_df)
    new_df <-
      purrr::map_dfr(
        mod_dfs,
        function(module) {
          new_module <- module

          I <- length(which(module$state==2))
          A <- length(which(module$state==4))
          avg_inf_contacted <- (params$BI*I + params$BA*A) * params$delta_t / nrow(module)
          vnum_inf_contacted <- stats::rpois(nrow(module), avg_inf_contacted)
          new_module$state[which(module$state==0 & vnum_inf_contacted>params$exp_thres)] <- 1
          new_module
        }
      )
    return(new_df)
  }
}

#' Exposes agents based on assigned roles
#' @param inc_df data frame containing agent data for a single inc
#' @param params list of parameters for infection dynamics

expose_leads <- function(inc_df, params) {
  if (!is.data.frame(agent_dat)) {
    stop("Input 'agent_dat' is not a data frame")
  }
  if (!is.list(params)) {
    stop("Input 'params' is not a list")
  }
  if (is.null(names(params))) {
    stop("List 'params' must have named elements")
  }
  if (!(names(params) == c("BA", "BI", "delta_t", "exp_thres"))) {
    stop("Named parameter is missing, check for BA, BI, delta_t, and exp_thres")
  }

  new_df <- inc_df
  I <- length(which(inc_df$state==2 & inc_df$role==1))
  A <- length(which(inc_df$state==4 & inc_df$role==1))
  n_leads <- length(which(inc_df$role==1))
  avg_inf_contacted <- (params$BI*I + params$BA*A) * params$delta_t / n_leads
  vnum_inf_contacted <- stats::rpois(nrow(inc_df), avg_inf_contacted)

  new_df$state[which(inc_df$state==0 & inc_df$role==1 & vnum_inf_contacted>params$exp_thres)] <- 1

  new_df
}
