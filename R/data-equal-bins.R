#' Reference bin catalogs derived from CSV exports.
#'
#' @name equal_bins_data
#' @docType data
#' @description These datasets mirror the CSV artifacts stored under
#'   `inst/extdata/` and are produced by `data-raw/prepare_binned_data.R`.
#'   Each table captures a fixed binning strategy so downstream visuals can be
#'   reproduced without rerunning the expensive samplers.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{Location}{Ordinal identifier of the bin within the sweep.}
#'   \item{Range_Min}{Lower bound of the input interval.}
#'   \item{Range_Max}{Upper bound of the input interval.}
#'   \item{Bin_Type}{Factor describing how the bins were created (equal width,
#'     varied bottoms, etc.).}
#'   \item{Magic}{Integer representation of the restoring constant.}
#'   \item{Avg_Relative_Error}{Average relative error over the interval.}
#'   \item{Max_Relative_Error}{Worst-case relative error within the interval.}
#'   \item{N}{Number of bins in the sweep.}
#' }
#' @source `data-raw/prepare_binned_data.R`
NULL

#' @rdname equal_bins_data
"equal05_20bins"

#' @rdname equal_bins_data
"equal_bins"

#' @rdname equal_bins_data
"manybins"

#' @rdname equal_bins_data
"too_many_bins"

#' @rdname equal_bins_data
"varied_bins"
