# ---- Transform module: Optimization ----------------------------------------
# Bridges the optimization sweep outputs from data-raw/pipelines/binned.R with
# the ggplot constructors in R/plots-optimization.R.

#' Prepare optimized parameter search results for plotting.
#' 
#' @inheritParams compute_result_block
#' @return Tibble describing optimization outcomes.
#' @seealso \code{run_binned_pipeline()} for regenerating the optimization
#'   sweeps and [optimization_plots] for plotting helpers.
#' @export
prep_optimized_results <- function(slices = 100000, GRID_SIZE = 50) {
  compute_result_block(slices = slices, GRID_SIZE = GRID_SIZE)
}
