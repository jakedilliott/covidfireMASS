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

#' Clean up res_gaccs == "None"
#' @param inc_data data frame containing daily incident assignments
#' @param inc_info Data frame containing information on all the incidents in a season
#' @export
clean_gacc <- function(inc_data, inc_info) {
  missing_gacc <- grepl("^None", inc_data$res_gacc)

  # retrieve first inc mobilization
  first_mobs <- find_first_mob(inc_data[missing_gacc, ])
  # change inc_id into inc_gacc
  new_res_gacc <- sapply(first_mobs, inc_id_to_gacc, inc_info = inc_info)
  # assign the inc_gacc to their res_gacc
  inc_data$res_gacc[missing_gacc] <- new_res_gacc

  as.character(inc_data$res_gacc)
}
