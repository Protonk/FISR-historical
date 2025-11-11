# ---- Plot module: Deconstruction visuals -----------------------------------
# These ggplot constructors build on the datasets materialised by
# data-raw/pipelines/deconstruction.R. Use them alongside the prep helpers in
# R/transform-deconstruction.R to move from pipeline outputs -> transformed
# tibbles -> visuals.

#' Deconstruction visualization builders.
#' 
#' These plotting helpers pair with the datasets returned by
#' \code{run_deconstruction_pipeline()}. Each function expects prepped data from
#' the `prep_deconstruction_*()` family and returns a [ggplot2::ggplot] object
#' ready for rendering.
#'
#' @param deconstructed Core deconstruction tibble.
#' @param narrowed Zoomed-in deconstruction tibble.
#' @param widened Wide sweep deconstruction tibble.
#' @param iter_levels Iteration levels retained when spotlighting subsets.
#' @param widened_data Output from [prep_deconstruction_combined()].
#' @return A [ggplot2::ggplot] object.
#' @name deconstruction_plots
#' @seealso \code{run_deconstruction_pipeline()} for regenerating the source
#'   datasets and [transform_deconstruction] for the companion transformation
#'   helpers.
NULL

#' @rdname deconstruction_plots
#' @export
deconstruction_zoom_plot <- function(narrowed) {
  zoomed <- prep_deconstruction_zoom(narrowed)

  ggplot2::ggplot(
    zoomed,
    ggplot2::aes(
      x = relative_error,
      y = magic
    )
  ) +
    ggplot2::geom_point(shape = ".", alpha = 0.9) +
    ggplot2::xlab("Relative Error") +
    ggplot2::ylab("Magic Constant") +
    ggplot2::labs(
      color = "Iterations\nto converge",
      title = "Zooming in on three similar constants"
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf("0x%X", as.integer(x)),
      limits = c(0x5f37642f - 3200, 0x5F376D60)
    ) +
    .mc_annotate(0x5f3759df, "0x5f3759df", "blue", x_end = 0.04) +
    .mc_annotate(0x5f37642f, "0x5f37642f", "red", x_end = 0.043) +
    .mc_annotate(0x5f375a86, "0x5f375a86", "orange") +
    .mc_annotate(0x5F376908, "0x5F376908", "purple") +
    ggplot2::xlim(-0.035, 0.08)
}

#' @rdname deconstruction_plots
#' @export
deconstruction_rate_plot <- function(deconstructed) {
  prepared <- prep_deconstruction_errors(deconstructed)
  pals <- .deconstruction_palettes(prepared)

  ggplot2::ggplot(
    prepared,
    ggplot2::aes(
      x = input,
      y = error_before,
      color = iters
    )
  ) +
    .create_geom_points(prepared, pals$iter_levels, 16, 2, 0.95) +
    ggplot2::guides(alpha = "none") +
    ggplot2::scale_color_manual(values = pals$iter_colors, breaks = 1:6) +
    ggplot2::labs(
      color = "Iteration\nCount",
      title = "Rate of convergence is not symmetric about first guess errors"
    ) +
    ggplot2::ylab("Error before NR Iteration") +
    ggplot2::xlab("Input float")
}

#' @rdname deconstruction_plots
#' @export
deconstruction_magic_span_plot <- function(deconstructed) {
  spans <- prep_deconstruction_magic_span(deconstructed)

  ggplot2::ggplot(
    spans,
    ggplot2::aes(x = iters, ymin = min_magic, ymax = max_magic)
  ) +
    ggplot2::scale_y_continuous(labels = function(x) sprintf("0x%X", as.integer(x))) +
    ggplot2::geom_errorbar(width = 0.5) +
    ggplot2::geom_point(ggplot2::aes(y = min_magic), color = "blue") +
    ggplot2::geom_point(ggplot2::aes(y = max_magic), color = "red") +
    ggplot2::labs(
      x = "Iterations until convergence",
      y = "Integer value",
      title = "Good constants exist only in a narrow range"
    ) +
    ggplot2::theme_minimal()
}

#' @rdname deconstruction_plots
#' @export
deconstruction_quadratic_plot <- function(deconstructed) {
  prepared <- prep_deconstruction_errors(deconstructed)
  pals <- .deconstruction_palettes(prepared)

  ggplot2::ggplot(
    prepared,
    ggplot2::aes(
      x = relative_error_before,
      y = relative_error_after,
      color = iter_rank
    )
  ) +
    ggplot2::geom_point(shape = 16, size = 0.8, alpha = 0.9) +
    ggplot2::scale_color_manual(
      values = pals$iter_rank_hue,
      guide = ggplot2::guide_legend(override.aes = list(size = 1.5))
    ) +
    ggplot2::labs(
      title = "NR converges quadratically",
      x = "Relative error before Newton-Raphson",
      y = "Relative error after one iteration",
      color = "Iterations\nuntil\neventual\nconvergence"
    )
}

#' @rdname deconstruction_plots
#' @export
deconstruction_improvement_plot <- function(deconstructed) {
  prepared <- prep_deconstruction_errors(deconstructed)
  pals <- .deconstruction_palettes(prepared)

  ggplot2::ggplot(
    prepared,
    ggplot2::aes(
      x = relative_error_before,
      y = improvement,
      color = iter_rank
    )
  ) +
    ggplot2::geom_point(shape = 16, size = 0.5) +
    ggplot2::scale_color_manual(
      values = pals$iter_rank_hue,
      guide = ggplot2::guide_legend(override.aes = list(size = 3))
    ) +
    ggplot2::labs(
      x = "Relative error of first guess",
      y = "Improvement from one Newton-Raphson step",
      color = "Iterations\nto convergence",
      title = "Plotted against relative improvement, optimal region is visible"
    )
}

#' @rdname deconstruction_plots
#' @export
deconstruction_painterly_plot <- function(deconstructed) {
  ggplot2::ggplot(
    deconstructed,
    ggplot2::aes(
      x = magic,
      y = input,
      color = iters
    )
  ) +
    .create_geom_points(deconstructed, 6:1, 16, 6) +
    ggplot2::guides(alpha = "none", color = "none", shape = "none", size = "none") +
    ggplot2::theme_void()
}

#' @rdname deconstruction_plots
#' @export
deconstruction_polar_plot <- function(deconstructed) {
  prepared <- prep_deconstruction_errors(deconstructed)

  ggplot2::ggplot(
    prepared,
    ggplot2::aes(
      x = error_before,
      y = after_one,
      color = iters
    )
  ) +
    ggplot2::geom_point(shape = ".") +
    ggplot2::guides(color = "none") +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::theme_void()
}

#' @rdname deconstruction_plots
#' @export
deconstruction_combined_plot <- function(deconstructed, widened, iter_levels = 1:5) {
  prepared <- prep_deconstruction_combined(deconstructed, widened, iter_levels)
  pals <- .deconstruction_palettes(deconstructed)

  subset_plot <- ggplot2::ggplot(
    prepared$subset,
    ggplot2::aes(
      x = relative_error,
      y = magic,
      color = iters
    )
  ) +
    ggplot2::geom_point(shape = 16, size = 0.65, alpha = 0.95) +
    ggplot2::labs(
      color = "Iterations\nto converge",
      title = "Shaded region is 0.024% of the 32 bit integers"
    ) +
    ggplot2::guides(colour = "none") +
    ggplot2::scale_color_manual(values = pals$iter_rank_hue[1:5]) +
    ggplot2::scale_x_continuous(breaks = c(0), limits = c(-0.25, 0.25)) +
    ggplot2::ylim(1.5935e9, 1601175552) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.45),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::annotate("rect", alpha = 0.2, fill = "blue",
                      xmin = -Inf, xmax = -0.125,
                      ymin = 0x5F300000, ymax = 0x5F400000) +
    ggplot2::annotate("rect", alpha = 0.2, fill = "blue",
                      xmin = 0.125, xmax = Inf,
                      ymin = 0x5F300000, ymax = 0x5F400000) +
    ggplot2::geom_hline(yintercept = 0x5F400000,
                        color = "blue", alpha = 0.6, lty = 4, linewidth = 0.2) +
    ggplot2::geom_hline(yintercept = 0x5F300000,
                        color = "blue", alpha = 0.6, lty = 4, linewidth = 0.2)

  ggplot2::ggplot(
    prepared$wide,
    ggplot2::aes(
      x = relative_error,
      y = magic,
      color = iter_rank
    )
  ) +
    ggplot2::geom_point(shape = 16, size = 0.65, alpha = 0.95, show.legend = TRUE) +
    ggplot2::xlab("Relative error") +
    ggplot2::ylab("Restoring constant (in billions)") +
    ggplot2::labs(
      color = "Iterations\nto converge",
      title = "The region where the approximation is optimal is tiny.",
      fill = "Range of\noptimal integers"
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf("%.1f", x / 1e9),
      limits = c(1.3e9 - 1, NA)
    ) +
    ggplot2::scale_color_manual(values = pals$iter_rank_hue, na.translate = FALSE, drop = FALSE) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 5))) +
    ggplot2::annotate("rect", alpha = 0.2, fill = "blue",
                      xmin = -Inf, xmax = -0.1,
                      ymin = 0x5F300000, ymax = 0x5F400000) +
    ggplot2::annotate("rect", alpha = 0.2, fill = "blue",
                      xmin = 0.1, xmax = Inf,
                      ymin = 0x5F300000, ymax = 0x5F400000) +
    ggplot2::annotate("rect",
                      fill = NA, color = "black",
                      xmin = -0.5, xmax = 0.5,
                      ymin = 1.325e9, ymax = 1.525e9) +
    ggplot2::annotate("segment", x = -0.15, xend = -0.5, y = 1602500000, yend = 1.525e9,
                      linetype = "dashed") +
    ggplot2::annotate("segment", x = 0.15, xend = 0.5, y = 1602500000, yend = 1.525e9,
                      linetype = "dashed") +
    ggplot2::annotate("rect", color = "black", fill = NA, linetype = "dashed",
                      xmin = -0.15, xmax = 0.15,
                      ymin = 1592500000, ymax = 1602500000) +
    ggplot2::annotate("segment", x = 0.15, xend = 0.5, y = 1592500000, yend = 1.325e9,
                      linetype = "dashed") +
    ggplot2::annotate("segment", x = -0.15, xend = -0.5, y = 1592500000, yend = 1.325e9,
                      linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(fill = "0x5F300000 to\n0x5F400000"), alpha = 0) +
    ggplot2::scale_fill_manual(values = c("0x5F300000 to\n0x5F400000" = "blue"),
                               name = "Optimal\ninteger values") +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(
      color = "blue",
      alpha = 0.2,
      size = 5,
      shape = 15
    ))) +
    ggplot2::annotation_custom(
      grob = ggplot2::ggplotGrob(subset_plot),
      xmin = -0.5, xmax = 0.5,
      ymin = 1.325e9, ymax = 1.525e9
    )
}
