# ---- Transform module: Deconstruction --------------------------------------
# Bridges between the deconstruction pipeline outputs (see
# data-raw/pipelines/deconstruction.R) and the ggplot builders defined in
# R/plots-deconstruction.R.

#' Prepare deconstruction-derived datasets for visualization.
#' 
#' These helpers enrich the raw outputs from [run_deconstruction_pipeline()] so
#' plotting functions can focus on layer composition. Each function returns a
#' tibble tailored to a family of deconstruction visuals.
#'
#' @param deconstructed Tidy tibble describing the core deconstruction sample.
#' @param narrowed Tidy tibble describing the zoomed-in deconstruction sample.
#' @param widened Tidy tibble describing the wide sweep deconstruction sample.
#' @param iter_levels Integer vector of iteration levels to retain.
#' @param max_iter Highest iteration count to include when summarising spans.
#' 
#' @return A tibble or named list ready for plotting.
#' @name transform_deconstruction
#' @seealso \code{run_deconstruction_pipeline()} for refreshing the underlying
#'   datasets and [deconstruction_plots] for the plotting helpers that consume
#'   these transforms.
NULL

#' @rdname transform_deconstruction
#' @export
prep_deconstruction_errors <- function(deconstructed) {
  deconstructed |>
    dplyr::mutate(
      error_before = initial - reference,
      error_after = after_one - reference,
      relative_error_before = error_before / reference,
      relative_error_after = error_after / reference,
      improvement = abs(error_before) / reference - abs(error_after) / reference
    )
}

#' @rdname transform_deconstruction
#' @export
prep_deconstruction_zoom <- function(narrowed) {
  narrowed |>
    dplyr::mutate(
      relative_error = (initial - reference) / reference
    )
}

#' @rdname transform_deconstruction
#' @export
prep_deconstruction_magic_span <- function(deconstructed, max_iter = 4) {
  deconstructed |>
    dplyr::filter(as.numeric(iters) <= max_iter) |>
    dplyr::group_by(iters) |>
    dplyr::summarise(
      min_magic = min(magic),
      max_magic = max(magic),
      .groups = "drop"
    )
}

#' @rdname transform_deconstruction
#' @export
prep_deconstruction_subset <- function(deconstructed, iter_levels = 1:5) {
  level_labels <- levels(deconstructed$iters)
  keep_levels <- level_labels[iter_levels]

  deconstructed |>
    dplyr::filter(iters %in% keep_levels) |>
    dplyr::mutate(
      relative_error = (initial - reference) / reference
    )
}

#' @rdname transform_deconstruction
#' @export
prep_deconstruction_combined <- function(deconstructed, widened, iter_levels = 1:5) {
  list(
    subset = prep_deconstruction_subset(deconstructed, iter_levels),
    wide = widened |>
      dplyr::mutate(
        relative_error = (initial - reference) / reference
      )
  )
}
