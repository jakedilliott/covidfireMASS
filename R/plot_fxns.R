# plotting functions

#' Plot runs of a single season
#' @param data Outputs from the simulation function
#' @param ... parameters to pass on to ggplot labs
#' @export
single_plot <- function(data, ...) {
  data$state <- factor(data$state, levels = c(0, 1, 2, 3, 4), labels = c("S", "E", "I", "R", "A"))

  to_plot <- dplyr::count(data, time, state, name = "count")

  ggplot2::ggplot(to_plot, ggplot2::aes(time, count, color = state)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(...)
}
