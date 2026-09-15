#plain-text settings files of the Shiny tools

test_that("settings round-trip through the text file with their types", {
  values <- list(
    count = 5000, whole = 3L, seed = -12L, text = "Poisson", flag = TRUE, missing = NA,
    missing_text = NA_character_, none = NULL, cap = Inf, negative = -2.5, tiny = 1e-300,
    huge = 1.7e308, thresholds = c(1000, 5000, 25000), ids = 1:4, named = c(mean = 3, sd = 1.5),
    query = "SELECT a, b\nFROM claims -- comment: with 'quotes' and \"double\" quotes",
    commas = "a, b; c: d", accented = "café"
  )
  file <- tempfile(fileext = ".dcf")
  on.exit(unlink(file), add = TRUE)
  write_settings_file(values, file, tool = "test tool", version = 3)
  back <- read_settings_file(file, tool = "test tool")
  expect_identical(back$tool, "test tool")
  expect_identical(back$version, 3)
  expect_identical(names(back$values), names(values))
  for (id in names(values)) expect_identical(back$values[[id]], values[[id]], info = id)
  #the file is plain text a person can read
  expect_true(any(grepl("^thresholds: c\\(1000, 5000, 25000\\)$", readLines(file))))
})

test_that("reading a settings file never evaluates code", {
  file <- tempfile(fileext = ".dcf")
  on.exit(unlink(file), add = TRUE)
  marker <- tempfile()
  attacks <- c(
    paste0("file.create('", gsub("\\\\", "/", marker), "')"),
    "system('echo hacked')", "(function() 1)()", "get('Sys.time')()", "c(1, stop('x'))",
    "quote(x)", "list(1, 2)", "`c`(Sys.time())", "1:1e9"
  )
  for (attack in attacks) {
    writeLines(c("NetSimRSettings: test tool", "SettingsVersion: 1", paste0("value: ", attack)), file)
    expect_error(read_settings_file(file, tool = "test tool"), "cannot be read", info = attack)
  }
  expect_false(file.exists(marker))
})

test_that("files that are not settings files of the tool are refused with a message", {
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  #an .rds file, as the tools saved before
  saveRDS(list(version = 2, inputs = list(a = 1)), file)
  expect_error(read_settings_file(file, tool = "test tool"), "not a NetSimR settings file")
  writeLines("just some text", file)
  expect_error(read_settings_file(file, tool = "test tool"), "not a NetSimR settings file")
  write_settings_file(list(a = 1), file, tool = "other tool", version = 1)
  expect_error(read_settings_file(file, tool = "test tool"), "for the other tool, not the test tool")
  writeLines(c("NetSimRSettings: test tool", "SettingsVersion: x"), file)
  expect_error(read_settings_file(file, tool = "test tool"), "no valid version")
  expect_error(write_settings_file(list(`bad name` = 1), file, "test tool", 1), "names")
  expect_error(write_settings_file(list(a = list(1)), file, "test tool", 1), "vectors")
})
