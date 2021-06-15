#' Assign each firefighter agent a role "type"
#'
#' Inspired by fizzbuzz, this function finds which module types each agent has
#' been on and prints them out
#'
#' @param mod_data Module assignment data
#'
#' @returns Character vector of agent role/module "types" (Overhead, OverheadCrew,
#'     OverheadEngine, OverheadCrewEngine, Crew, CrewEngine, Engine)
#' @export
#'
#' @examples
#' \dontrun{
#' find_role_type(mod_id_2017)
#' }
find_role_type <- function(mod_data) {
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                                   total = nrow(mod_data) )

  sapply(1:nrow(mod_data),
         function(row)
         {
           agent_mods <- unique(as.list(mod_data[row, -(1:2)]))
           agent_type <- find_mod(agent_mods)
           pb$tick()
           return(agent_type)
         })
}

is_mod <- function(data, mod) {
  mod_expr <- paste0("^", mod, "-") # "^O-", "^C-", "^E-"
  grep_call <- grep(mod_expr, data)

  if (length(grep_call) > 0) {
    stats::setNames(TRUE, mod)
  } else {
    stats::setNames(FALSE, mod)
  }
}

find_mod <- function(data) {
  mods <- unlist(lapply(c("O", "C", "E"), function(x) is_mod(data, x)))

  out <- ""
  if (mods["O"]) out <- paste0(out, "Overhead")
  if (mods["C"]) out <- paste0(out, "Crew")
  if (mods["E"]) out <- paste0(out, "Engine")

  return(out)
}
