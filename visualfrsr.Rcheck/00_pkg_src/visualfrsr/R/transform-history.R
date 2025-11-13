# ---- Transform module: Historical comparisons ------------------------------
# Helpers that convert historical approximation samples into tidy tibbles for
# the plots in R/plots-history.R. The raw samples originate from
# data-raw/pipelines/deconstruction.R when the approximated artifacts are
# refreshed.

#' Prepare historical approximation datasets for plotting.
#' 
#' @inheritParams generate_example_samples
#' @return Tidy tibble containing approximated inverse square roots.
#' @seealso \code{run_deconstruction_pipeline()} for regenerating the
#'   approximated datasets and [history_plots] for visual layers that consume the
#'   tidy output.
#' @export
prep_example_samples <- function(
    slices = 16384,
    float_tol = 0.0004882812) {
  generate_example_samples(slices = slices, float_tol = float_tol)
}
