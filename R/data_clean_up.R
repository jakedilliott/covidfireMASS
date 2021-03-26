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
  missing_gacc <- grepl("^None", inc_data$res_gacc)

  # retrieve first incident mobilization
  first_mobs <- find_first_mob(inc_data[missing_gacc, ])
  # change inc_id into inc_gacc
  new_res_gacc <- sapply(first_mobs, inc_id_to_gacc, inc_info = inc_info)
  # assign the inc_gacc to their res_gacc
  inc_data$res_gacc[missing_gacc] <- new_res_gacc

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
