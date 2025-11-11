#' Sample representative clusters for downstream visualizations.
#'
#' @param n Number of samples per cluster.
#' @param cluster_ranges List of length three containing magic ranges.
#' @return Tibble with input/error/magic/cluster columns.
#' @export
sample_clusters <- function(
    n = 16384,
    cluster_ranges = list(
      c(1597622357, 1597176038),
      c(1598049921, 1597918602),
      c(1597884263, 1596910052)
    )) {
  representative_clusters <- purrr::map2_dfr(
    cluster_ranges,
    seq_along(cluster_ranges),
    function(bounds, cluster_id) {
      frsr_sample(
        n = n,
        NRmax = 1,
        keep_params = TRUE,
        x_min = 0.5,
        x_max = 2.0,
        magic_max = bounds[1],
        magic_min = bounds[2]
      ) |>
        dplyr::select(input, error, magic) |>
        dplyr::mutate(cluster = factor(cluster_id))
    }
  )

  representative_clusters
}
