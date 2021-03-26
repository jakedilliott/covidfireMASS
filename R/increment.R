#' Increment operator
#'
#' used to increment days in quarantine
#'
#' @param e1 ...
#' @param e2 ...
#' @export
'%+=%' <- function(e1, e2) {
  eval.parent(substitute(e1 <- e1 + e2))
}
