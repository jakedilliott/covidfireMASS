# Cleaning up modules

#' Group all overhead modules for each incident into 1 module
#'
#' Solves the problem where incidents had too many unique overhead
#' modules by changing the all overhead modules to "O-100".
#'
#' @param mod_id_df module id assignment data frame
#' @returns all overhead assignments are changed to "O-100"
#' @export
clean_mods <- function(mod_id_df) {
  purrr::map_dfc(
    mod_id_df,
    function(x) {
      overheads <- grepl("^O-", x)
      x[overheads] <- "O-100"
      x
    }
  )
}


# Cleaning up gaccs =====

#' Clean up missing gaccs
#'
#' Agents with missing home gaccs have their home gacc assigned to the gacc of
#' the first fire of the season.
#'
#' @param inc_data data frame containing daily incident assignments
#' @param inc_info data frame containing information on all the incidents of a season
#' @export
clean_gacc <- function(inc_data, inc_info) {
  missing_gacc <- grep("^None", inc_data$res_gacc)

  if (length(missing_gacc) > 0) {
    # retrieve first incident mobilization
    first_mobs <- find_first_mob(inc_data[missing_gacc, ])
    # change inc_id into inc_gacc
    new_res_gacc <- sapply(first_mobs, inc_id_to_gacc, inc_info = inc_info)
    cat("New gacc length: ", length(new_res_gacc)," | Missing gacc length: ", length(missing_gacc), "\n")
    # assign the inc_gacc to their res_gacc
    inc_data$res_gacc[missing_gacc] <- new_res_gacc
  }

  as.character(inc_data$res_gacc)
}

#' Find the first mobilization for an agent/res_id
#' @param res_data A data frame with
find_first_mob <- function(res_data) {
  out <- apply(
    res_data, 1,
    function(agent) {
      vMobs <- as.vector(agent[-(1:2)], "numeric")
      for (inc in 1:length(vMobs)) {
        if (vMobs[inc] > 0) {
          return(vMobs[inc])
        }
      }
    }
  )
  unlist(out)
}

#' Translate inc_id into inc_gacc
#' @param inc_id numeric incident ID
#' @param inc_info data frame containing seasonal information on incidents
inc_id_to_gacc <- function(inc_id, inc_info) {
  if (inc_id == 0) {
    "None"
  } else {
    inc_info$inc_gacc[inc_info$inc_id == inc_id]
  }
}

#' Count daily stats
#'
#' @param data Daily sim results
count_daily_mwf <- function(data) {
  out <- dplyr::count(data, time, inc_id, quarantine, vaccinated, state)
  out <- tidyr::pivot_wider(out, names_from = "state",
                            values_from = "n", values_fill = 0)
  states <- c("S", "E", "I", "A", "R")
  if (ncol(out) < 9) {
    missing <- states[!(states %in% names(out))]
    for (i in missing) {
      out <- dplyr::mutate(out, missing = 0)
      names(out)[length(out)] <- i
    }
  }

  return(out)
}

#' Count daily new infections
#'
#' @param agent_df data frame from previous day
#' @param new_df data frame from current/new day
count_daily_inf <- function(new_df, agent_df) {
  new_inf <- new_df[new_df$state == "E" & agent_df$state == "S", ]
  out <- dplyr::count(new_inf, time, inc_id, quarantine, vaccinated, name = "new_inf")

  return(out)
}
