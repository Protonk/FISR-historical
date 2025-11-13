# ---- Plot module: Cluster visuals ------------------------------------------
# This module exposes ggplot constructors for the clustering artifacts created
# via data-raw/pipelines/clusters.R. Pair these layers with the outputs from
# R/transform-clusters.R to move from pipeline data -> transformed tibbles ->
# visuals.

#' Cluster visualization builders.
#' 
#' Plot sampled clusters of magic constants across the input range. These
#' layers assume data produced by \code{run_cluster_pipeline()} and tidied by
#' helpers in \code{R/transform-clusters.R}.
#' 
#' @param clusters Tibble produced by [prep_cluster_samples()].
#' @return A [ggplot2::ggplot] object visualising representative clusters.
#' @seealso \code{run_cluster_pipeline()} for regenerating the source datasets
#'   and [prep_cluster_samples()] for data preparation helpers.
#' @export
plot_sampled_clusters <- function(clusters = prep_cluster_samples()) {
  ggplot2::ggplot(
    clusters,
    ggplot2::aes(x = input, y = error, color = cluster, group = cluster)
  ) +
    ggplot2::geom_point(shape = ".") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ stats::poly(x, 25), se = FALSE) +
    ggplot2::labs(
      title = "Representative cluster bands for FRSR magic constants",
      x = "Input value",
      y = "Relative error",
      color = "Cluster"
    )
}
