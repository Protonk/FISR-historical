# ---- Transform module: Binning ---------------------------------------------
# Data wrangling helpers that bridge between the binning pipeline outputs and
# plotting layers. Each helper assumes data refreshed via
# data-raw/pipelines/binned.R and readied for plotting by the functions below.

#' Prepare binned data products for visualization.
#' 
#' These helpers wrap higher level binning utilities so plots can focus on
#' aesthetic composition. Functions return ready-to-plot data frames or lists of
#' derived statistics produced after \code{run_binned_pipeline()}.
#'
#' @param bucket_df Output from [load_bucket_sweep()].
#' @param selection_df Data frame returned by \code{bucket_selection()}.
#' @param spread_df Data frame containing error spreads for paired plots.
#' @param binned Dataset of optimal bucket summaries.
#' @param df Binned dataset used when recomputing bucket selections.
#' @param n_values Vector of `N` values to retain.
#' @param n_small Small bucket count used for comparison visuals.
#' @param n_large Large bucket count used for comparison visuals.
#' @param bins Integer vector of bin counts to evaluate.
#' @param range Vector of candidate bucket counts.
#' @param error Which error metric to compare (`"max"` or `"avg"`).
#' @param bucket1 First bucket selection tibble supplied to the comparator.
#' @param bucket2 Second bucket selection tibble supplied to the comparator.
#' 
#' @return A tibble or list ready for downstream plotting.
#' @name transform_binning
#' @seealso \code{run_binned_pipeline()} for regenerating the source data and
#'   [binning_plots] for the visual layers that consume these transforms.
NULL

#' @rdname transform_binning
#' @export
prep_bucket_heatmap <- function(bucket_df = load_bucket_sweep()) {
  build_heatmap_data(bucket_df)
}

#' @rdname transform_binning
#' @export
prep_bucket_selection <- function(df = load_bucket_sweep(), range = 4:100) {
  bucket_selection(df, range = range)
}

#' @rdname transform_binning
#' @export
prep_bucket_widths <- function(selection_df) {
  selection_df |>
    dplyr::mutate(
      M = factor(M),
      order = factor(order),
      N = factor(N)
    )
}

#' @rdname transform_binning
#' @export
prep_bucket_rectangles <- function(selection_df) {
  selection_df |>
    dplyr::mutate(
      M_index = as.numeric(factor(M))
    )
}

#' @rdname transform_binning
#' @export
prep_bucket_pair_layout <- function(bucket_df, spread_df) {
  y_range <- range(as.numeric(factor(bucket_df$M)))

  list(
    buckets = bucket_df |>
      dplyr::mutate(M_index = as.numeric(factor(M))),
    spread = spread_df |>
      dplyr::mutate(
        error_scaled = scales::rescale(error, to = y_range + c(-0.4, 0.4))
      ),
    y_range = y_range
  )
}

#' @rdname transform_binning
#' @export
prep_bucket_comparison <- function(binned, n_small, n_large) {
  small <- dplyr::filter(binned, N == n_small)
  large <- dplyr::filter(binned, N == n_large)
  match_index <- findInterval(large$Range_Min, small$Range_Min)
  match_index[match_index == 0] <- 1

  large <- dplyr::mutate(
    large,
    matched_avg_error = small$Avg_Error[pmin(match_index, nrow(small))]
  )

  combined <- binned |>
    dplyr::filter(N %in% c(n_small, n_large)) |>
    tidyr::pivot_wider(
      names_from = N,
      values_from = c(Range_Min, Range_Max, Avg_Error),
      names_sep = "_"
    )

  list(
    small = small,
    large = large,
    combined = tidyr::drop_na(combined)
  )
}

#' @rdname transform_binning
#' @export
prep_bucket_error_segments <- function(df) {
  segments_h <- df |>
    tidyr::pivot_longer(
      cols = c(Avg_Error, Max_Error),
      names_to = "Error_Type",
      values_to = "Error"
    ) |>
    dplyr::mutate(
      Error_Type = factor(
        Error_Type,
        levels = c("Avg_Error", "Max_Error"),
        labels = c("Avg Error", "Max Error")
      )
    )

  segments_v <- dplyr::bind_rows(
    df |>
      dplyr::transmute(
        x = Range_Max,
        y_start = Avg_Error,
        y_end = Max_Error
      ),
    df |>
      dplyr::transmute(
        x = Range_Min,
        y_start = Avg_Error,
        y_end = Max_Error
      )
  )

  list(horizontal = segments_h, vertical = segments_v)
}

#' @rdname transform_binning
#' @export
prep_bucket_multiple_n <- function(binned, n_values = unique(binned$N)) {
  segments_h <- binned |>
    dplyr::filter(N %in% n_values) |>
    tidyr::pivot_longer(
      cols = c(Avg_Error, Max_Error),
      names_to = "Error_Type",
      values_to = "Error"
    ) |>
    dplyr::mutate(
      Error_Type = factor(
        Error_Type,
        levels = c("Avg_Error", "Max_Error"),
        labels = c("Avg Error", "Max Error")
      )
    )

  segments_v <- dplyr::bind_rows(
    binned |>
      dplyr::filter(N %in% n_values) |>
      dplyr::group_by(N) |>
      dplyr::slice(1:(dplyr::n() - 1)) |>
      dplyr::transmute(
        N = N,
        x = Range_Max,
        y_start = Avg_Error,
        y_end = Max_Error
      ),
    binned |>
      dplyr::filter(N %in% n_values) |>
      dplyr::group_by(N) |>
      dplyr::slice(2:dplyr::n()) |>
      dplyr::transmute(
        N = N,
        x = Range_Min,
        y_start = Avg_Error,
        y_end = Max_Error
      )
  )

  list(horizontal = segments_h, vertical = segments_v)
}

#' @rdname transform_binning
#' @export
prep_bucket_error_curve <- function(binned, bins = 4:36) {
  purrr::map_dfr(
    bins,
    ~tibble::tibble(bins = .x, error = norm_errorN(binned, .x))
  )
}

#' @rdname transform_binning
#' @export
prep_bucket_compare_buckets <- function(bucket1, bucket2, error = "max") {
  error_col <- if (error == "max") "Max_Error" else "Avg_Error"

  x_points <- sort(unique(c(
    bucket1$Range_Min, bucket1$Range_Max,
    bucket2$Range_Min, bucket2$Range_Max
  )))

  rectangles <- tibble::tibble(
    xmin = x_points[-length(x_points)],
    xmax = x_points[-1]
  ) |>
    dplyr::mutate(
      error1 = sapply(xmin, function(x) {
        bucket1[[error_col]][x >= bucket1$Range_Min & x < bucket1$Range_Max][1]
      }),
      error2 = sapply(xmin, function(x) {
        bucket2[[error_col]][x >= bucket2$Range_Min & x < bucket2$Range_Max][1]
      }),
      ymin = pmin(error1, error2),
      ymax = pmax(error1, error2),
      fill_color = dplyr::if_else(error2 < error1, "blue", "red")
    )

  segments_h1 <- bucket1 |>
    dplyr::select(Range_Min, Range_Max, Error = !!rlang::sym(error_col))

  segments_h2 <- bucket2 |>
    dplyr::select(Range_Min, Range_Max, Error = !!rlang::sym(error_col))

  list(rectangles = rectangles, bucket1 = segments_h1, bucket2 = segments_h2)
}
