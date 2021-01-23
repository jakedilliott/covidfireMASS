#' Module based quarantine operations
#' @param input_list list of data frames split by fire module
#' @export
modular_quarantine <- function(input_list, pIR, pAQ) {
  out <- lapply(
    input_list,
    function(module) {
      rQ <- runif(nrow(module))

      nQ <- length(which((module$state == 2 && rQ < 1-pIR) || (module$state == 4 && rQ < pAQ)))

      if ((nQ) > 0) {
        if ("O-100" %in% module$mod_id) {
          module$res_id[which((module$state == 2 && rQ < 1-pIR) || (module$state == 4 && rQ < pAQ))]
        } else {
          module$res_id
        }
      }
    }
  )
  as.vector(unlist(out))
}
