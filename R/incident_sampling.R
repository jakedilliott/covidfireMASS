# covidfireMASS/R/incident_sampling.R
# written by Jake Dilliott
# March 2021

#' Sample specific incident data from seasonal data
#'
#' @param inc_id_data seasonal incident id assignment data
#' @param inc_info_data seasonal incident information data
#' @param inc_name desired incident to extract, must be the same case as
#'     `inc_info_data$inc_name` (so likely in all caps)
#'
#' @return data frame; only columns and rows that contain the desired incident
#' @export
#'
#' @examples
#' \dontrun{
#' inc_id_highline <- sample_inc_id(inc_id_2017, inc_info_2017, 'HIGHLINE')
#' }
sample_inc_id <- function(inc_id_data, inc_info_data, inc_name) {
  inc_info <- get_inc_info(inc_info_data, inc_name)
  start_col <- which(names(inc_id_data) == inc_info$start)
  end_col <- which(names(inc_id_data) == inc_info$end)

  agents_on_inc <- apply(
    inc_id_data,
    1, # rows
    function(x) inc_info$inc_id %in% x
  )

  out <- inc_id_data[agents_on_inc, c(1:2, start_col:end_col)]
  filter_desired_inc(out, inc_info$inc_id)
}

#' Sample specific incident data from module assignment data
#'
#' @param inc_id_template inc_id data frame that has been sampled for the disired
#'     incident; output from `sample_inc_id()`
#' @param mod_id_data seasonal module id assignment data
#'
#' @return data frame; same dimensions and columns/rows as the inc_id_template
#' @export
#'
#' @examples
#' \dontrun{
#' inc_id_highline <- sample_inc_id(inc_id_2017, inc_info_2017, 'HIGHLINE')
#' mod_id_highline <- sample_mod_id(inc_id_highline, mod_id_2017)
#' }
sample_mod_id <- function(inc_id_template, mod_id_data) {
  start_col <- which(names(mod_id_data) == names(inc_id_template)[3])
  end_col <- which(names(mod_id_data) == utils::tail(names(inc_id_template), 1))
  out <- mod_id_data[mod_id_data$res_id %in% inc_id_template$res_id,
                       c(1:2, start_col:end_col)]

  purrr::map2_dfc(
    inc_id_template,
    out,
    function(template, mod_ids) {
      ifelse(template == 0, 0, mod_ids)
    }
  )
}

get_inc_info <- function(inc_info_data, inc_name) {
  inc_index <- grep(inc_name, inc_info_data$inc_name)
  inc_id <- inc_info_data$inc_id[inc_index]
  start <- inc_info_data$first_day[inc_index]
  end <- inc_info_data$last_day[inc_index]

  list(inc_id=inc_id, start=as.character(start), end=as.character(end))
}

filter_desired_inc <- function(data, inc_id) {
  cleaned_cols <- purrr::map_dfc(
    data[, -(1:2)],
    function(col) ifelse(col == inc_id, inc_id, 0)
  )
  dplyr::bind_cols(data[, 1:2], cleaned_cols)
}

