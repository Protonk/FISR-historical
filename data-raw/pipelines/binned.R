if (!exists("bucket_selection")) {
  source(file.path("R", "binned.R"), local = FALSE)
}

#' Run the binning pipeline to regenerate histogram-friendly datasets.
#'
#' @param bin_sizes Integer vector of bin counts evaluated by
#'   `frsrr::frsr_bin()`.
#' @param x_min Lower bound of the sampled input domain.
#' @param x_max Upper bound of the sampled input domain.
#' @param bucket_range Integer sequence describing the bucket sizes retained by
#'   [bucket_selection()].
#'
#' @param save_to_data Should the generated tibbles be serialized into
#'   `.rda` files under `data/`?
#' @param data_dir Output directory for serialized `.rda` files.
#' @return Invisibly returns a list containing three tibbles: `bin_samples`
#'   (raw output from [`frsrr::frsr_bin()`] across the requested bin sizes),
#'   `bucket_summary` (optimised coverage bands), and `oob_performance`
#'   (average error when using a bucketed magic constant outside its home
#'   interval).
run_binned_pipeline <- function(
    bin_sizes = c(4, 6, 8, 12, 18, 24, 32, 48, 64, 96, 128),
    x_min = 1.0,
    x_max = 2.0,
    bucket_range = 4:100,
    save_to_data = TRUE,
    data_dir = "data") {
  frsr_bin_multi <- function(sizes, x_min, x_max) {
    floats <- 16384
    ints <- 524288
    NRmax <- 1
    magic_min <- 1596980000L
    magic_max <- 1598050000L

    bins <- lapply(sizes, function(n) {
      frsrr::frsr_bin(
        n_bins = n,
        NRmax = NRmax,
        x_min = x_min,
        x_max = x_max,
        float_samples = floats,
        magic_samples = ints,
        magic_min = magic_min,
        magic_max = magic_max
      )
    })

    dplyr::bind_rows(bins)
  }

  binned <- frsr_bin_multi(bin_sizes, x_min, x_max) |>
    tibble::as_tibble()

  bucket_summary <- bucket_selection(binned, range = bucket_range) |>
    tibble::as_tibble()

  oob <- compute_oob_performance(binned)

  outputs <- list(
    bin_samples = binned,
    bucket_summary = bucket_summary,
    oob_performance = oob
  )

  if (isTRUE(save_to_data)) {
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = TRUE)
    }

    for (name in names(outputs)) {
      save(
        list = name,
        file = file.path(data_dir, paste0(name, ".rda")),
        compress = "xz",
        envir = list2env(stats::setNames(list(outputs[[name]]), name), parent = emptyenv())
      )
    }
  }

  invisible(outputs)
}
