#' Run the deconstruction sampling pipeline.
#'
#' This pipeline regenerates the three datasets that power the
#' deconstruction visualizations. Each dataset is sampled from
#' [`frsrr::frsr_sample()`] with defaults mirroring the interactive
#' exploration widgets.
#'
#' @param tolerance Convergence tolerance passed to `frsr_sample()`.
#' @param NRmax Iteration ceiling for the core and wide samples.
#' @param narrow_NRmax Iteration ceiling for the zoomed-in sample.
#' @param magic_core Length-two integer vector giving the magic range for
#'   the core sample.
#' @param magic_wide Length-two integer vector giving the magic range for
#'   the wide sweep sample.
#' @param magic_narrow Length-two integer vector giving the magic range for
#'   the zoomed-in sample.
#' @param n_core Number of observations drawn for the core sample.
#' @param n_wide Number of observations drawn for the wide sweep sample.
#' @param n_narrow Number of observations drawn for the zoomed-in sample.
#'
#' @param save_to_data Should the generated tibbles be serialized into
#'   `.rda` files under `data/`?
#' @param data_dir Output directory for serialized `.rda` files.
#' @return Invisibly returns a named list of tibbles (`deconstructed`,
#'   `widened`, `narrowed`) that are ready to be serialized to `data/`.
run_deconstruction_pipeline <- function(
    tolerance = 2^-15,
    NRmax = 95,
    narrow_NRmax = 2,
    magic_core = c(1593500000, 1601175552),
    magic_wide = c(1300000000, 1602500000),
    magic_narrow = c(1597461647, 1597469647),
    n_core = 524288,
    n_wide = 262144,
    n_narrow = 131072,
    save_to_data = TRUE,
    data_dir = "data") {
  sample_block_rows <- function(df, block_size = 2048) {
    limit <- nrow(df) - (nrow(df) %% block_size)
    if (limit <= 0) {
      return(df)
    }
    df[sample(limit), ]
  }

  prep_deconstruction_df <- function(df, NRmax) {
    df |>
      sample_block_rows() |>
      dplyr::filter(iters <= (NRmax - 1)) |>
      dplyr::mutate(
        reference = 1 / sqrt(input),
        iters = factor(iters, levels = 1:max(iters), labels = 1:max(iters)),
        input_rank = dplyr::ntile(input, 128),
        magic_rank = dplyr::ntile(magic, 128),
        initial_rank = dplyr::ntile(initial - reference, 128),
        after_rank = dplyr::ntile(after_one - reference, 128),
        iter_rank = cut(
          as.numeric(iters),
          breaks = c(0, 1, 2, 3, 4, 24, 60, 94),
          labels = c("1", "2", "3", "4", "5-24", "25-60", "61-94")
        )
      ) |>
      tibble::as_tibble()
  }

  deconstructed <- frsrr::frsr_sample(
    n = n_core,
    NRmax = NRmax,
    tol = tolerance,
    magic_min = magic_core[1],
    magic_max = magic_core[2],
    keep_params = TRUE
  )

  widened <- frsrr::frsr_sample(
    n = n_wide,
    NRmax = NRmax,
    tol = tolerance,
    magic_min = magic_wide[1],
    magic_max = magic_wide[2],
    keep_params = TRUE
  )

  narrowed <- frsrr::frsr_sample(
    n = n_narrow,
    NRmax = narrow_NRmax,
    magic_min = magic_narrow[1],
    magic_max = magic_narrow[2],
    keep_params = TRUE
  ) |>
    dplyr::mutate(reference = 1 / sqrt(input)) |>
    tibble::as_tibble()

  outputs <- list(
    deconstructed = prep_deconstruction_df(deconstructed, NRmax),
    widened = prep_deconstruction_df(widened, NRmax),
    narrowed = narrowed
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
