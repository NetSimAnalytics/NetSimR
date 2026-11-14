#Browser test of the header on wide screens (helpers in helper-app.R): the reserving tool has
#the most tabs, eight, and between 992 and 1600 px they squeezed its name out of sight (the
#brand shrank to make room for links that never wrap). Its name now keeps its width, and the
#links move to a second row where they do not fit beside it.

test_that("the reserving tool shows its whole name beside or above its tabs at every width", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("reserving", "reserving-navbar", width = 1300, height = 900)
  on.exit(app$stop(), add = TRUE)
  app_idle(app)
  for (width in c(1000, 1100, 1250, 1300, 1366, 1500, 1600, 1700, 1920)) {
    app$set_window_size(width, 900)
    app_idle(app)
    header <- app$get_js("(function () {
      var title = document.querySelector('.sim-brand-title');
      var row = document.querySelector('.navbar > .container-fluid');
      var links = Array.prototype.map.call(document.querySelectorAll('.navbar .nav-link'),
        function (a) { return a.getBoundingClientRect().height; });
      return {shown: title.clientWidth, full: title.scrollWidth, overflow: row.scrollWidth - row.clientWidth,
              tallest_link: Math.max.apply(null, links)};
    })()")
    #the name is not cut, the header does not overflow, and no link label wraps onto two lines
    expect_gte(header$shown, header$full - 1, label = paste("the width of the name at", width, "px"))
    expect_lte(header$overflow, 0, label = paste("the overflow of the header at", width, "px"))
    expect_lt(header$tallest_link, 50, label = paste("the height of a link at", width, "px"))
  }
  expect_app_logs_clean(app)
})
