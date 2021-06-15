#' Plan vaccines based on gacc information
#'
#' Uses the approx function to calculate the number of agents that need to be
#' vaccinated during each time step rounded to the nearest whole number.
#'
#' @param table Contingency table created by "table" containing frequency of
#'     either gaccs or roles
#' @param x Numeric vector, time steps
#' @param y Numeric vector, values from 0-1 specifying proportion of each gacc
#'   to vaccinate at times corresponding to x
#'
#' @return Vaccine plan where row = time, colnames = gacc, values = number
#'   of agents to vaccinate
#' @export
#'
#' @examples
#' \dontrun{
#' agent_roles <- find_role_types(mod_id_2017)
#' role_table <- table(ifelse(grepl("Overhead", agent_roles), "overhead", "ground"))
#' vax_plan <- plan_vaccines(role_table,
#'                           x = c(1, 154, 354),
#'                           y = c(0.25, 0.50, 0.7))
#' }
plan_vaccines <- function(table, x, y) {
  if (length(x) < 2 | length(y) < 2) {
    stop("Inputs 'x' and 'y' must be length 2 or greater")
  }
  if (length(x) != length(y)) {
    stop("Inputs 'x' and 'y' must have equal length")
  }

  vax_plan <- lapply(
    table,
    function(pop) {
      to_vax <- pop * y
      round(stats::approx(x = x, y = to_vax, n = utils::tail(x, 1))$y)
    }
  )
  vax_plan <- as.data.frame(vax_plan)
  names(vax_plan) <- names(table)

  if ("overhead" %in% names(table)) {
    list(method = "role", plan = vax_plan)
  } else {
    list(method = "gacc", plan = vax_plan)
  }
}

#' Find out which agents to vaccinate using gacc information
#'
#' @param input_df Data frame containing agent roster
#' @param overhead_ids Res ids of overhead agents for the purpose of vaccination
#' @param method Method of vaccine delivery, either by role or by gacc
#' @param plan Vaccination plan
#' @param efficacy Proportion of vaccinated agents the gain immunity
#'
#' @returns Nested list of res_ids: output$vaccinated and output$immune
#' @importFrom rlang .data
vaccinate <- function(input_df, overhead_ids = NULL, method, plan, efficacy) {
  if (method == "role") {
    if (is.null(overhead_ids)) {
      stop("Overhead agents not specified")
    }
    input_df$role <- ifelse(input_df$res_id %in% overhead_ids, "overhead", "ground")
    split_df <- dplyr::group_split(input_df, .data$role)
  } else if (method == "gacc") {
    split_df <- dplyr::group_split(input_df, .data$res_gacc)
  } else {
    stop("Incorrect vaccine distribution method specified")
  }

  vax_ids <- lapply(
    split_df,
    function(sub_pop) {
      num_vaccinated <- length(which(sub_pop$vaccinated))
      candidates <- sub_pop$res_id[!sub_pop$vaccinated &
                                     !sub_pop$quarantine &
                                     sub_pop$inc_id == 0]

      if (method == "role") {
        role <- sub_pop$role[1]
        num_to_vax <- plan[1, role] - num_vaccinated
      } else if (method == "gacc") {
        home_gacc <- sub_pop$res_gacc[1]
        num_to_vax <- plan[1, home_gacc] - num_vaccinated
      }

      if (length(candidates) < num_to_vax) {
        candidates
      } else {
        sample(candidates, num_to_vax)
      }
    }
  )
  vax_ids <- as.numeric(unlist(vax_ids))
  # This method is deterministic, it guarantees that the proportion of
  # immune agents is equal to the vax_efficacy
  immune_ids <- sample(vax_ids, round(length(vax_ids) * efficacy))

  return(list("vaccinated" = vax_ids, "immune" = immune_ids))
}
