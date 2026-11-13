#CITATION.cff (GitHub's "Cite this repository") is kept out of the built package, so this runs
#only from the source folder: its version must be that of DESCRIPTION

test_that("CITATION.cff has the package's version", {
  cff <- test_path("..", "..", "CITATION.cff")
  skip_if_not(file.exists(cff), "CITATION.cff is only in the source folder")
  lines <- readLines(cff, warn = FALSE)
  version <- sub("^version:\\s*", "", grep("^version:", lines, value = TRUE))
  expect_identical(version, as.character(utils::packageVersion("NetSimR")))
  expect_true(any(grepl("^cff-version: 1\\.2\\.0$", lines)))
})
