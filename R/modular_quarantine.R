#' Module based quarantine operations
#'
#' @param input_df the agent data frame at time t
#' @param pIQ proportion of infectious symptomatic agents that do not recognize
#'   symptoms and therefore do not quarantine
#' @param pAQ proportion if infections asymptomatic agents that are caught by
#'   testing or camp screening and therefore quarantine
#'
#' @returns list of res_ids that will be quarantined at time t+1
#' @export
#' @importFrom rlang .data
modular_quarantine <- function(input_df, pIQ, pAQ) {
  split_by_mod <- dplyr::group_split(input_df, .data$inc_id, .data$mod_id)

  out <- lapply(split_by_mod,
                function(mod_df)
                {
                  rQ <- stats::runif(nrow(mod_df))
                  caught <- c(
                    mod_df$res_id[mod_df$state == "I" & rQ < pIQ],
                    mod_df$res_id[mod_df$state == "A" & rQ < pAQ]
                  )
                  if (length(caught) > 0L) {
                    if (0 %in% mod_df$inc_id | "O-100" %in% mod_df$mod_id) {
                      mod_df$res_id[caught]
                    } else {
                      mod_df$res_id
                    }
                  }
                })
  as.numeric(unlist(out))
}
