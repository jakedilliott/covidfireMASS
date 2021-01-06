#' Group overhead modules into 1 module
#' @param agent_df data frame containing firefighter agents data
#'
clean_mods <- function(agent_df) {
  if (!is.data.frame(agent_df)) {
    stop("Input is not a data frame")
  }
  new_df <- agent_df
  index_overhead <- stringr::str_which(new_df$mod_id, "O-")
  if (length(index_overhead > 0)) {
    new_df$module[index_overhead] <- "O-1"
  }
  new_df
}

#' Initializes the agent data frame
#'
#' @param inc_req data frame containing incident assignments
#' @param mod_req data frame containing module assignments
#' @param day day of the season to use as the initial data frame, default is 1
#' @export

mk_agents <- function(inc_req, mod_req, day = 1) {
  if (!is.data.frame(inc_req)) {
    stop("Input 'inc_req' is not a data frame")
  }
  if (!is.data.frame(mod_req)) {
    stop("Input 'mod_req' is not a data frame")
  }
  if (!is.numeric(day)) {
    stop("Input 'day' is not numeric")
  }

  N <- nrow(inc_req)
  agent_tib <- dplyr::select(inc_req, res_id, res_gacc)
  agent_tib <- dplyr::mutate(agent_tib,
    inc_id = dplyr::pull(inc_req, day + 2),
    mod_id = dplyr::pull(mod_req, day + 2),
    module = dplyr::pull(mod_req, day + 2),
    role = rep(0, N),
    state = rep(0, N),
    q_status = rep(0, N),
    days_q = rep(0, N)
  )
  agent_tib <- dplyr::arrange(
    agent_tib,
    dplyr::desc(inc_id),
    dplyr::desc(mod_id)
  )

  output <- clean_mods(agent_tib)

  dplyr::arrange(output, res_id)
}


#' Subset agent data frame by module
#' @param agent_df data frame containing agent data
#' @return A list object, each entry is a tibble containing data for an individual
#' module.
mod_subsetter <- function(agent_df) {
  if (!is.data.frame(agent_df)) {
    stop("Input is not a data frame")
  }
  inc_ls <- unique(agent_df$inc_id)
  mod_ls <- unique(agent_df$module)
  cross_df <- tidyr::crossing(inc_ls, mod_ls)

  full_list <- purrr::map2(
    cross_df$inc_ls, cross_df$mod_ls,
    function(x, y) {
      dplyr::filter(agent_df, inc_id == x, module == y)
    }
  )

  # don't include tibbles with no values
  full_list[sapply(full_list, nrow) > 0]
}

#' Subset agent dataset by incident
#' @param agent_df data frame containing agent data

inc_subsetter <- function(agent_df) {
  if (!is.data.frame(agent_df)) {
    stop("Input is not a data frame")
  }

  inc_ls <- unique(agent_df$inc_id)
  out_ls <- purrr::map(
    inc_ls,
    function(x) {
      dplyr::filter(agent_df, inc_id == x)
    }
  )
  out_ls
}

#' Assign roles to agents
#' @param agent_df data frame containing agent data
#'
assign_roles <- function(agent_df) {
  if (!is.data.frame(agent_df)) {
    stop("Input is not a data frame")
  }
  modules_ls <- mod_subsetter(agent_df)

  out_tibble <-
    purrr::map_dfr(
      modules_ls,
      function(module_data) {
        new_df <- module_data
        if (0 %in% module_data$inc_id) {
          return(new_df)
        } else {
          if (!(1 %in% module_data$role)) {
            module_n <- nrow(module_data)
            if (module_n == 1) {
              new_df$role[1] <- 1
            }
            if (module_n > 1) {
              new_df$role[sample(module_n, 1)] <- 1
            }
          }
          return(new_df)
        }
      }
    )
  out_tibble
}

#' Move agents between fires
#' @param agent_df data frame containing agent data
#' @param inc_req data frame containing incident requests for agents
#' @param mod_req data frame containing module requests for agents
#' @param t numeric time value
#' @param eir Entry Infection Rate, numeric 0<=x<=1, or NULL

mv_agents <- function(agent_df, inc_req, mod_req, t, eir = 0) {
  sapply(
    list(agent_df, inc_req, mod_req),
    function(x) {
      if (!is.data.frame(x)) {
        stop(paste0("Input '", x, "' is not a data frame"))
      }
    }
  )
  if (!is.numeric(t)) {
    stop("Input 't' is not numeric")
  }
  if (!is.numeric(eir)) {
    stop("Input 'eir' is not numeric")
  }
  if (eir < 0 | eir >= 1) {
    stop("eir must be between 0 and 1")
  }

  new_df <- agent_df
  new_df$inc_id <- dplyr::pull(inc_req, t + 2)
  new_df$mod_id <- dplyr::pull(mod_req, t + 2)

  new_df$role[which(new_df$inc_id != agent_df$inc_id)] <- 0
  new_df <- clean_mods(new_df)

  if (!is.null(eir)) {
    probE <- stats::runif(nrow(agent_df))
    new_df$state[which(agent_df$state == 0 & agent_df$inc_id != new_df$inc_id & probE < eir)]
  }

  new_df
}
