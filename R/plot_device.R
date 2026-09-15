################
#Plot device for the Shiny tools
################

# Extra device arguments for shiny::renderPlot(). Shiny draws plots with ragg, the macOS
# quartz device or the Cairo package when they are available, and otherwise with
# grDevices::png(), whose default device on Windows cannot draw semi-transparent pixels:
# on a transparent background it loses text anti-aliasing and faint colours, so charts in
# the dark theme look smeared. The cairo type of png() draws them properly, so it is asked
# for only in that case (the other devices do not take a type argument), with greyscale
# anti-aliasing: its default, subpixel, gives text coloured fringes.
plot_device_args <- function() {
  installed <- function(pkg) nzchar(system.file(package = pkg))
  uses_ragg <- isTRUE(getOption("shiny.useragg", TRUE)) && installed("ragg")
  uses_cairo_package <- isTRUE(getOption("shiny.usecairo", TRUE)) && installed("Cairo")
  uses_png <- !uses_ragg && !isTRUE(capabilities("aqua")) && !uses_cairo_package
  if (uses_png && .Platform$OS.type == "windows" && isTRUE(capabilities("cairo"))) {
    list(type = "cairo", antialias = "gray")
  } else {
    list()
  }
}

# shiny::renderPlot() for the tools' charts: redrawn when the output is resized (instead
# of rescaling the old image, which overlaps legends laid out for another width), and drawn
# with the device arguments above. Other arguments (bg, alt, height, ...) are passed on.
netsimr_render_plot <- function(expr, ..., env = parent.frame()) {
  args <- c(
    list(expr = substitute(expr), env = env, quoted = TRUE, execOnResize = TRUE),
    list(...),
    plot_device_args()
  )
  do.call(shiny::renderPlot, args, quote = TRUE)
}
