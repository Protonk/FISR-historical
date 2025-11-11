# ---- Plot module: Optimization visuals -------------------------------------
# Visual summaries of the Newton-Raphson parameter sweep. These functions rely
# on the datasets produced by data-raw/pipelines/binned.R and the transforms in
# R/transform-optimization.R to collapse raw grids into plotting-friendly
# structures.

#' Optimization visualization builders.
#' 
#' @param optimized Tibble produced by [prep_optimized_results()].
#' @return A [ggplot2::ggplot] object summarizing optimization landscapes.
#' @name optimization_plots
#' @seealso \code{run_binned_pipeline()} for regenerating the optimization
#'   landscapes and [prep_optimized_results()] for producing the tidy search
#'   results consumed by these plots.
NULL

#' @rdname optimization_plots
#' @export
plot_optimized_tile <- function(optimized = prep_optimized_results()) {
  ggplot2::ggplot(optimized, ggplot2::aes(x = A_rank, y = B_rank, fill = err_rank)) +
    ggplot2::geom_tile() +
    ggplot2::labs(
      title = "Heatmap of NR parameter errors",
      x = "A rank",
      y = "B rank",
      fill = "Error rank"
    )
}

#' @rdname optimization_plots
#' @export
plot_optimized_paths <- function(optimized = prep_optimized_results()) {
  ggplot2::ggplot(
    optimized,
    ggplot2::aes(
      x = input,
      y = error,
      color = pair
    )
  ) +
    ggplot2::geom_path() +
    ggplot2::guides(color = "none") +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::theme_void()
}

#' @rdname optimization_plots
#' @export
plot_optimized_pairs <- function(optimized = prep_optimized_results()) {
  ggplot2::ggplot(optimized, ggplot2::aes(x = A, y = B, color = error)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Parameter search landscape",
      x = "Half-three parameter",
      y = "Half-one parameter",
      color = "Error"
    )
}
