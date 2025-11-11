if (!exists("sample_clusters")) {
  source(file.path("R", "clustered.R"), local = FALSE)
}

#' Run the clustering pipeline used to build band and sample datasets.
#'
#' @param n_floats Number of floating point inputs drawn for each sweep when
#'   forming the cluster bands.
#' @param n_ints Number of integer magics evaluated per sweep when forming the
#'   cluster bands.
#' @param cluster_ranges A list of length three defining the inclusive magic
#'   ranges for the representative cluster samples.
#' @param samples_per_cluster Number of points sampled for each cluster in the
#'   representative dataset.
#'
#' @param save_to_data Should the generated tibbles be serialized into
#'   `.rda` files under `data/`?
#' @param data_dir Output directory for serialized `.rda` files.
#' @return Invisibly returns a list containing two tibbles: `cluster_bands`
#'   summarises the upper and lower error envelopes for each cluster, while
#'   `representative_clusters` contains point samples used in the orbit
#'   visualizations.
run_cluster_pipeline <- function(
    n_floats = 2048,
    n_ints = 16384,
    cluster_ranges = list(
      c(1597622357, 1597176038),
      c(1598049921, 1597918602),
      c(1597884263, 1596910052)
    ),
    samples_per_cluster = 16384,
    save_to_data = TRUE,
    data_dir = "data") {
  cluster_magic_constants <- function(data, n_clusters = 3) {
    grouped <- data |>
      dplyr::group_by(magic) |>
      dplyr::summarize(
        mean_error = mean(error),
        min_error = min(error),
        max_error = max(error),
        .groups = "drop"
      )

    kmeans_result <- stats::kmeans(
      grouped[, c("mean_error", "min_error", "max_error")],
      centers = n_clusters
    )
    grouped$cluster <- kmeans_result$cluster

    grouped |>
      dplyr::group_by(cluster) |>
      dplyr::summarize(
        average_magic = mean(magic),
        min_magic = min(magic),
        max_magic = max(magic),
        count = dplyr::n(),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = c(average_magic, min_magic, max_magic),
        names_to = "statistic",
        values_to = "value"
      ) |>
      dplyr::mutate(
        statistic = factor(
          statistic,
          levels = c("average_magic", "min_magic", "max_magic"),
          labels = c("Average", "Minimum", "Maximum")
        )
      ) |>
      tibble::as_tibble()
  }

  build_cluster_bands <- function(n_floats, n_ints) {
    low_err <- frsrr::frsr_sample(
      n_floats = n_floats,
      NRmax = 1,
      keep_params = TRUE,
      x_min = 0.5,
      x_max = 2.0,
      n_ints = n_ints,
      magic_min = 1596980000L,
      magic_max = 1598050000L
    ) |>
      dplyr::select(input, error, magic)

    wide_err <- frsrr::frsr_sample(
      n_floats = n_floats,
      NRmax = 1,
      keep_params = TRUE,
      x_min = 0.5,
      x_max = 2.0,
      n_ints = n_ints,
      magic_min = 1596910000L,
      magic_max = 1598100000L
    ) |>
      dplyr::select(input, error, magic)

    low_clusters <- cluster_magic_constants(low_err) |>
      dplyr::filter(cluster %in% c(1, 2))

    high_cluster <- cluster_magic_constants(wide_err) |>
      dplyr::filter(cluster == 1) |>
      dplyr::mutate(cluster = 3)

    dplyr::bind_rows(low_clusters, high_cluster) |>
      dplyr::filter(statistic != "Average") |>
      dplyr::select(!count) |>
      tibble::as_tibble()
  }

  cluster_bands <- build_cluster_bands(n_floats, n_ints)
  representative_clusters <- sample_clusters(
    n = samples_per_cluster,
    cluster_ranges = cluster_ranges
  ) |>
    tibble::as_tibble()

  outputs <- list(
    cluster_bands = cluster_bands,
    representative_clusters = representative_clusters
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
