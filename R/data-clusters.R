#' Cluster exploration artifacts
#'
#' These datasets originate from `run_cluster_pipeline()` in
#' `data-raw/pipelines/clusters.R` and power the cluster-oriented
#' visualizations in the package.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{cluster}{Cluster identifier produced by `stats::kmeans()`.}
#'   \item{statistic}{Whether the row captures the minimum or maximum error.}
#'   \item{value}{Magic constant associated with the statistic.}
#' }
#' @source `data-raw/pipelines/clusters.R`
"cluster_bands"

#' @rdname cluster_bands
#' @format A tibble with columns:
#' \describe{
#'   \item{input}{Sampled floating-point inputs.}
#'   \item{error}{Approximation error for each sample.}
#'   \item{magic}{Magic constant applied to the sample.}
#'   \item{cluster}{Factor indicating the cluster membership.}
#' }
#' @source `data-raw/pipelines/clusters.R`
"representative_clusters"
