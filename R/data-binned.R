#' Binned optimisation artifacts
#'
#' Output from `run_binned_pipeline()` in `data-raw/pipelines/binned.R`, used by
#' the histogram visualizations and bucket selection helpers.
#'
#' @format A tibble combining sweeps across requested bin counts with columns:
#' \describe{
#'   \item{N}{Number of bins in the sweep.}
#'   \item{Range_Min}{Lower bound of the bin's input interval.}
#'   \item{Range_Max}{Upper bound of the bin's input interval.}
#'   \item{Magic}{Magic constant expressed as a hexadecimal string.}
#'   \item{Avg_Relative_Error}{Average relative error over the interval.}
#'   \item{Max_Relative_Error}{Worst-case relative error over the interval.}
#' }
#' Additional columns emitted by `frsrr::frsr_bin()` are preserved.
#' @source `data-raw/pipelines/binned.R`
"bin_samples"

#' @rdname bin_samples
#' @format A tibble with summary columns:
#' \describe{
#'   \item{M}{Target number of buckets to keep.}
#'   \item{Location}{Ordinal identifier of the bin location.}
#'   \item{N}{Number of bins evaluated for the location.}
#'   \item{Depth}{Relative depth statistic produced by `bucket_selection()`.}
#'   \item{Error}{Maximum relative error for the bucket.}
#'   \item{Variance}{Variance contribution for the bucket.}
#'   \item{Width}{Interval width in the input domain.}
#'   \item{Midpoint}{Midpoint of the input interval.}
#'   \item{Left}{Left edge of the interval.}
#'   \item{Rarity}{Relative frequency of observing the bucket.}
#'   \item{order}{Ordering of buckets within a given `M`.}
#' }
#' @source `data-raw/pipelines/binned.R`
"bucket_summary"

#' @rdname bin_samples
#' @format A tibble that extends `bin_samples` with an additional column:
#' \describe{
#'   \item{OOB_Error}{Average error when the bucket is applied outside its interval.}
#' }
#' @source `data-raw/pipelines/binned.R`
"oob_performance"
