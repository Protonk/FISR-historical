# ---- Plot utilities --------------------------------------------------------
# Internal helpers shared across visual families. These functions do not export
# and exist solely to support the topic-specific plotting modules.

.deconstruction_palettes <- function(df) {
  iter_levels <- as.integer(levels(df$iters))
  iter_colors <- grDevices::colorRampPalette(c("dodgerblue2", "red"))(length(iter_levels))
  iter_rank_hue <- c("lightblue", grDevices::colorRampPalette(c("white", "orange1", "red"))(7))

  list(
    iter_levels = rev(iter_levels),
    iter_colors = stats::setNames(iter_colors, seq_along(iter_colors)),
    iter_rank_hue = iter_rank_hue
  )
}

.mc_annotate <- function(magic_value, label,
                         color, x_start = -0.035, x_end = 0.036,
                         text_size = 8) {
  list(
    ggplot2::annotate(
      "segment",
      x = x_start, xend = x_end,
      y = magic_value, yend = magic_value,
      color = color, linetype = 2, linewidth = 1.5
    ),
    ggplot2::annotate(
      "point",
      x = x_end,
      y = magic_value,
      color = color,
      size = 3
    ),
    ggplot2::annotate(
      "text",
      x = x_end + 0.002,
      y = magic_value,
      label = label,
      hjust = -0.05,
      vjust = 0.5,
      color = color,
      size = text_size
    )
  )
}

.create_geom_points <- function(data, iter_range, shape, size, alpha = 1) {
  lapply(iter_range, function(i) {
    ggplot2::geom_point(
      data = data[data$iters == i, ],
      shape = shape,
      size = size,
      alpha = alpha
    )
  })
}

.blinncomp_labels <- c(
  "Blinn" = "Blinn\n(1997)",
  "QuakeIII" = "Quake III\n(1999)",
  "Moroz" = "Moroz\n(2016)",
  "Kahan" = "Kahan\n(1999)"
)
