# ---- Transform module: Clusters --------------------------------------------
# Helpers for tidying the clustering pipeline outputs from
# data-raw/pipelines/clusters.R so that plot functions can focus on
# presentation.

#' Prepare sampled clusters for plotting.
#' 
#' Wraps [sample_clusters()] so ggplot builders can accept either raw cluster
#' draws or pre-computed data. The function simply forwards arguments to the
#' sampler and returns the tidy tibble, bridging from
#' \code{run_cluster_pipeline()} to [plot_sampled_clusters()].
#'
#' @inheritParams sample_clusters
#' @return Tibble of sampled clusters.
#' @seealso \code{run_cluster_pipeline()} for regenerating the underlying data
#'   and [plot_sampled_clusters()] for visualizing the prepared samples.
#' @export
prep_cluster_samples <- function(
    n = 16384,
    cluster_ranges = list(
      c(1597622357, 1597176038),
      c(1598049921, 1597918602),
      c(1597884263, 1596910052)
    )) {
  sample_clusters(n = n, cluster_ranges = cluster_ranges)
}

