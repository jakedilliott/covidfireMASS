#' Module based quarantine operations
#' @param input_list list of data frames split by fire module
#' @export
modular_quarantine <- function(input_df, pIR, pAQ) {
  splitby_mod <- split(input_df, paste0(input_df$inc_id, "_", input_df$mod_id))

  out <- lapply(
    splitby_mod,
    function(mod_df) {
      rQ <- runif(nrow(mod_df))
      caught <- which((mod_df$state == "I" & rQ < 1-pIR) |
                        (mod_df$state == "A" & rQ < pAQ))
      if (length(caught) > 0) {
        if (0 %in% mod_df$inc_id | "O-100" %in% mod_df$mod_id) {
          mod_df$res_id[caught]
        } else {
          mod_df$res_id
        }
      }
    }
  )
  as.numeric(unlist(out))
}
