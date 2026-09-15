#plot device used by the charts of the Shiny tools

test_that("the cairo type is asked for only when shiny would use the Windows png() device", {
  args <- plot_device_args()
  expect_type(args, "list")
  uses_other_device <- nzchar(system.file(package = "ragg")) || nzchar(system.file(package = "Cairo")) ||
    isTRUE(capabilities("aqua"))
  if (.Platform$OS.type == "windows" && !uses_other_device && isTRUE(capabilities("cairo"))) {
    expect_identical(args, list(type = "cairo"))
  } else {
    expect_identical(args, list())
  }
  #switching shiny's preferred packages off cannot make the arguments invalid for png()
  old <- options(shiny.useragg = FALSE, shiny.usecairo = FALSE)
  on.exit(options(old), add = TRUE)
  expect_true(length(plot_device_args()) %in% c(0, 1))
})

test_that("netsimr_render_plot draws the chart lazily, redraws on resize and passes its arguments on", {
  drawn <- 0
  server <- function(input, output, session) {
    output$chart <- netsimr_render_plot({
      drawn <<- drawn + 1
      graphics::plot(1:10, col = grDevices::adjustcolor("steelblue", 0.4), pch = 19)
    }, bg = "transparent", alt = "A test chart")
  }
  #the chart code must not run when the render function is created
  expect_equal(drawn, 0)
  shiny::testServer(server, {
    session$setInputs(.clientdata_output_chart_width = 400, .clientdata_output_chart_height = 300)
    img <- output$chart
    expect_match(img$src, "^data:image/png;base64,")
    expect_identical(img$alt, "A test chart")
    expect_gte(drawn, 1)
  })
})
