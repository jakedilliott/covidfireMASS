# plotting functions

#' Plot runs of a single season
#' @param data Outputs from the simulation function
#' @param ... parameters to pass on to ggplot labs
#' @export
single_plot <- function(data, ...) {
  data$state <- factor(data$state, levels = c(0, 1, 2, 3, 4), labels = c("S", "E", "I", "R", "A"))

  to_plot <- dplyr::count(data, time, state, name = "count")

  ggplot2::ggplot(to_plot, ggplot2::aes(time, count)) +
    ggplot2::geom_area(ggplot2::aes(fill = state), size = 1.2, position = "stack") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(...)
}

plot_quarantined <- function(data, ...) {
 data$q_status <- factor(data$q_status, levels = c(0,1), labels = c("Non_Q", "Q"))

 to_plot <- dplyr::count(data, time, q_status, name = "count")

 ggplot2::ggplot(to_plot, ggplot2::aes(time, count)) +
   ggplot2::geom_area(ggplot2::aes(fill = q_status), size = 1.2, position = "stack") +
   ggplot2::theme_bw(base_size = 14) +
   ggplot2::scale_fill_brewer(palette = "Set2") +
   ggplot2::labs(...)
}

plot_by_gacc <- function(data, ...) {
  filter_inf <- dplyr::filter(data, state > 0)
  to_plot <- dplyr::count(filter_inf, time, res_gacc, name = "count")

  ggplot2::ggplot(to_plot, ggplot2::aes(time, count)) +
    ggplot2::geom_col(ggplot2::aes(fill = res_gacc), position = "stack") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(...)
}
