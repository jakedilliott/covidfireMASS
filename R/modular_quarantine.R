#' Module based quarantine operations
#' @param inc_df data frame containing the data for a single incident
#' @param new_df data frame to be modified

modular_quarantine <- function(inc_df, new_df) {
  if (inf_df[["inc_id"]][1] > 0){
    inc_split <- split(inc_df, inc_df[["mod_id"]])
    new_split <- split(new_df, new_df[["mod_id"]])

    new_df <- purrr::map2_dfr(
      inc_split, new_split,
      function(module, new_module) {
        if (length(which(module$q_status == 1)) > 0) {
          new_module$q_status <- 1
        }
        new_module
      }
    )
    return(new_df)
  } else {
    return(new_df)
  }
}
