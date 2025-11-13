generator_vis <- function(draw = 200, x_max = 16) {
  x <- frsr_sample(n = draw, method = "log_stratified", x_max = x_max)$input
  y <- frsr_sample(n = draw, method = "irrational",      x_max = x_max)$input
  par(mai = c(0, 0, 0, 0))  # no margins
  smoothScatter(
    x = y,
    y = x,
    xlim = rev(range(y)),
    nbin = 64,
    axes = FALSE,
    ann = FALSE,
    asp = 1
  )
}

render_generator_vis <- function(
    nframes = 400,
    duration = 2,
    out_file = "scatter.mp4",
    draw = 200,
    x_max = 16) {
  fps <- nframes / duration

  av::av_capture_graphics(
    {
      for (i in seq_len(nframes)) {
        generator_vis(draw = draw, x_max = x_max)
      }
    },
    output = out_file,
    framerate = fps,
    width = 800,   # square frames to match asp=1
    height = 800,
    res = 144
  )

  if (interactive()) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::viewer(out_file)
    } else {
      utils::browseURL(out_file)
    }
  }
}
