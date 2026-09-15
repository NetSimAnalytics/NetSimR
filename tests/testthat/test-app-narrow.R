#Browser tests of the three tools at a phone's width (helpers in helper-app.R): the header
#fits, and no page scrolls sideways

expect_fits_narrow_window <- function(tool, tab) {
  skip_if_no_app_browser()
  app <- start_netsimr_app(tool, paste0(tool, "-narrow"), width = 360, height = 800)
  on.exit(app$stop(), add = TRUE)
  app_idle(app)

  #the navigation collapses behind the menu button, which is inside the window
  toggler <- app$get_js("(function () {
    var t = document.querySelector('.navbar-toggler, .navbar-toggle');
    if (!t || t.offsetParent === null) return {shown: false};
    var box = t.getBoundingClientRect();
    return {shown: true, left: box.left, right: box.right};
  })()")
  expect_true(toggler$shown)
  expect_gte(toggler$left, 0)
  expect_lte(toggler$right, 360)

  expect_no_horizontal_scroll(app, paste0("The welcome page of the ", tool))
  app_goto(app, tab)
  expect_no_horizontal_scroll(app, paste0("The ", tab, " tab of the ", tool))
  expect_app_logs_clean(app)
}

test_that("the simulator fits a 360 px window", {
  expect_fits_narrow_window("simulator", "simulator")
})

test_that("the distribution fitting tool fits a 360 px window", {
  expect_fits_narrow_window("distribution", "data")
})

test_that("the GLM fitting tool fits a 360 px window", {
  expect_fits_narrow_window("glm", "model")
})
