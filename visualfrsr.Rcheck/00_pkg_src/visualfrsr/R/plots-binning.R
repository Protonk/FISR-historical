# ---- Plot module: Binning visuals ------------------------------------------
# This file holds user-facing ggplot constructors for binning explorations.
# Pair these helpers with the datasets generated via
# `run_binned_pipeline()` (data-raw/pipelines/binned.R) and the transforms in
# R/transform-binning.R to travel from raw samples -> plotting layers.

#' Binning visualization builders.
#' 
#' Assemble ggplot layers for the binned datasets and helpers. These
#' functions expect inputs prepared by [transform_binning] and ultimately draw
#' from the artifacts refreshed via \code{run_binned_pipeline()}.
#' 
#' @name binning_plots
#' @seealso \code{run_binned_pipeline()} for producing the datasets that feed
#'   these plots and [transform_binning] for the supporting data wrangling
#'   helpers.
NULL

#' @rdname binning_plots
#' @param bucket_df Data frame returned by [load_bucket_sweep()].
#' @return A [ggplot2::ggplot] heatmap of bucket selections.
#' @export
bucket_heatmap_plot <- function(bucket_df = load_bucket_sweep()) {
  heatmap_ready <- prep_bucket_heatmap(bucket_df)

  ggplot2::ggplot(heatmap_ready,
                  ggplot2::aes(x = factor(Location), y = Magic, fill = factor(N))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::theme_void() +
    ggplot2::guides(fill = "none")
}

#' @rdname binning_plots
#' @param selection_df Tibble from [prep_bucket_selection()].
#' @export
plot_bucket_widths <- function(selection_df = prep_bucket_selection()) {
  prepared <- prep_bucket_widths(selection_df)

  ggplot2::ggplot(prepared,
                  ggplot2::aes(x = M,
                               y = Width,
                               group = order,
                               fill = N)) +
    ggplot2::geom_col(color = "black", linewidth = 0.05) +
    ggplot2::guides(color = "none", fill = "none") +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::scale_fill_discrete(type = sample(viridis::turbo(length(unique(prepared$N)))))
}

#' @rdname binning_plots
#' @export
plot_bucket_widths_horizontal <- function(selection_df = prep_bucket_selection()) {
  prepared <- prep_bucket_widths(selection_df)

  ggplot2::ggplot(prepared,
                  ggplot2::aes(y = M,
                               x = Width,
                               group = order,
                               fill = N)) +
    ggplot2::geom_col(color = "black", linewidth = 0.05) +
    ggplot2::guides(color = "none", fill = "none") +
    ggplot2::scale_fill_discrete(type = sample(viridis::turbo(length(unique(prepared$N)))))
}

#' @rdname binning_plots
#' @export
plot_bucket_rectangles <- function(selection_df = prep_bucket_selection()) {
  prepared <- prep_bucket_rectangles(selection_df)

  ggplot2::ggplot(prepared,
                  ggplot2::aes(ymin = M_index - 0.5,
                               ymax = M_index + 0.5,
                               xmin = Left,
                               xmax = Left + Width,
                               fill = factor(N))) +
    ggplot2::geom_rect(color = "black", linewidth = 0.1) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0.5, 1.5)) +
    ggplot2::scale_y_continuous(
      breaks = 4:max(prepared$M),
      labels = unique(prepared$M),
      expand = c(0, 0),
      limits = c(12, NA)
    ) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::theme_void() +
    ggplot2::labs(x = "Position", y = "M", fill = "N") +
    ggplot2::guides(fill = "none")
}

#' @rdname binning_plots
#' @param bucket_df Bucket definitions for the binned view.
#' @param spread_df Error spread tibble for overlayed scatter/smooth layers.
#' @export
pair_plot <- function(bucket_df, spread_df) {
  prepared <- prep_bucket_pair_layout(bucket_df, spread_df)

  ggplot2::ggplot() +
    ggplot2::geom_rect(data = prepared$buckets,
                       ggplot2::aes(ymin = M_index - 0.5,
                                    ymax = M_index + 0.5,
                                    xmin = Left,
                                    xmax = Left + Width,
                                    fill = factor(N)),
                       color = "black", linewidth = 0.1) +
    ggplot2::scale_fill_discrete(type = viridis::turbo(prepared$y_range[2])) +
    ggplot2::scale_x_continuous(limits = c(0.5, 2.0)) +
    ggplot2::scale_y_continuous(
      name = "Bucket (M)",
      breaks = unique(prepared$buckets$M_index),
      labels = unique(prepared$buckets$M)
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::geom_point(data = prepared$spread,
                        ggplot2::aes(x = input, y = error_scaled, color = cluster),
                        shape = 16, size = 0.2, alpha = 0.4) +
    ggplot2::geom_smooth(
      data = prepared$spread,
      ggplot2::aes(x = input, y = error_scaled, color = cluster, group = cluster),
      method = "lm", formula = y ~ stats::poly(x, 25), se = FALSE
    ) +
    ggplot2::labs(x = "Input value", y = "Bucket (M)")
}

#' @rdname binning_plots
#' @param selection_df Data frame created by [prep_bucket_selection()].
#' @export
plot_bucket <- function(selection_df = prep_bucket_selection()) {
  prepared <- prep_bucket_error_segments(selection_df)

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = prepared$horizontal,
                          ggplot2::aes(x = Range_Min, xend = Range_Max,
                                       y = Error, yend = Error,
                                       color = Error_Type),
                          linewidth = 0.5) +
    ggplot2::geom_segment(data = prepared$vertical,
                          ggplot2::aes(x = x, xend = x,
                                       y = y_start, yend = y_end),
                          linetype = "dotted",
                          color = "black",
                          linewidth = 0.25,
                          alpha = 0.75) +
    ggplot2::scale_color_manual(values = c("Avg Error" = "blue", "Max Error" = "red")) +
    ggplot2::labs(x = "Input Value",
                  y = "Error") +
    ggplot2::scale_x_continuous(breaks = seq(0.25, 1, by = 0.25),
                                limits = c(0.25, 1.0)) +
    ggplot2::theme_minimal()
}

#' @rdname binning_plots
#' @param binned Binned dataset of optimal selections.
#' @param n_values Bucket counts retained in the facets.
#' @export
plot_multiple_n <- function(binned, n_values = unique(binned$N)) {
  prepared <- prep_bucket_multiple_n(binned, n_values)

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = prepared$horizontal,
                          ggplot2::aes(x = Range_Min, xend = Range_Max,
                                       y = Error, yend = Error,
                                       color = Error_Type),
                          linewidth = 0.5) +
    ggplot2::geom_segment(data = prepared$vertical,
                          ggplot2::aes(x = x, xend = x,
                                       y = y_start, yend = y_end),
                          linetype = "dotted",
                          color = "black",
                          linewidth = 0.25,
                          alpha = 0.75) +
    ggplot2::facet_grid(cols = ggplot2::vars(N)) +
    ggplot2::scale_color_manual(values = c("Avg Error" = "blue", "Max Error" = "red")) +
    ggplot2::labs(title = "Error Trends Across Input Range",
                  x = "Input Value",
                  y = "Error") +
    ggplot2::scale_x_continuous(breaks = seq(0.25, 1, by = 0.25),
                                limits = c(0.25, 1.0)) +
    ggplot2::theme_minimal()
}

#' @rdname binning_plots
#' @param binned Binned dataset from [run_binned_pipeline()].
#' @param bins Candidate bin counts for evaluation.
#' @export
plot_normalized_error_curve <- function(binned, bins = 4:36) {
  curve <- prep_bucket_error_curve(binned, bins)

  ggplot2::ggplot(curve, ggplot2::aes(x = bins, y = error)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Bins",
      y = "Normalized Error",
      title = "Optimal bucket selection error reduction slows after 24 bins"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(min(bins), max(bins), by = 4))
}

#' @rdname binning_plots
#' @param binned Dataset with multiple bucket counts.
#' @param n_small Smaller bucket count for comparison.
#' @param n_large Larger bucket count for comparison.
#' @export
compare_n_values <- function(binned, n_small, n_large) {
  prepared <- prep_bucket_comparison(binned, n_small, n_large)

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = prepared$small,
                          ggplot2::aes(x = Range_Min, xend = Range_Max,
                                       y = Avg_Error, yend = Avg_Error),
                          color = "red", linewidth = 0.5) +
    ggplot2::geom_segment(data = prepared$large,
                          ggplot2::aes(x = Range_Min, xend = Range_Max,
                                       y = Avg_Error, yend = Avg_Error),
                          color = "blue", linewidth = 0.5) +
    ggplot2::geom_rect(data = prepared$large,
                       ggplot2::aes(xmin = Range_Min, xmax = Range_Max,
                                    ymin = Avg_Error,
                                    ymax = matched_avg_error),
                       fill = "blue", alpha = 0.3) +
    ggplot2::labs(title = sprintf("Comparison of Avg Relative Error N=%d vs N=%d",
                                  n_small, n_large),
                  x = "Input Value",
                  y = "Avg Relative Error")
}

#' @rdname binning_plots
#' @param bucket1 First bucket selection tibble.
#' @param bucket2 Second bucket selection tibble.
#' @param error Error column to compare (`"max"` or `"avg"`).
#' @export
compare_buckets <- function(bucket1, bucket2, error = "max") {
  prepared <- prep_bucket_compare_buckets(bucket1, bucket2, error)

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = prepared$bucket1,
                          ggplot2::aes(x = Range_Min, xend = Range_Max,
                                       y = Error, yend = Error),
                          color = "black", linewidth = 0.5) +
    ggplot2::geom_segment(data = prepared$bucket2,
                          ggplot2::aes(x = Range_Min, xend = Range_Max,
                                       y = Error, yend = Error),
                          color = "black", linewidth = 0.5) +
    ggplot2::geom_rect(data = prepared$rectangles,
                       ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax,
                                    fill = fill_color),
                       alpha = 0.3) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(breaks = seq(0.25, 1, by = 0.25),
                                limits = c(0.25, 1.0)) +
    ggplot2::labs(title = "Comparison of Error Values Between Base and Extension",
                  x = "Input Value",
                  y = paste(error, "Relative Error")) +
    ggplot2::theme_minimal()
}
