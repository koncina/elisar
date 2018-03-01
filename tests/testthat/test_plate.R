context("read_plate")

test_that("reading non existing files generates a specific error message", {
  expect_error(read_plate(c("", "nofile")), "does not exist in current working directory")
})

test_that("reading NA or numericals generates an error", {
  expect_error(read_plate(c(NA, 1)), "^invalid 'file' argument$")
})

test_that("reading a valid excel file returns the appropriate dataframe", {
  .df <- read_plate("example_full.xls")
  expect_is(.df, "data.frame")
  expect_equal(colnames(.df), c("file", "sheet", "element", "row", "col", "id", "value", "description", "treatment", "medium"))
  expect_equal(nrow(.df), 96)
})

test_that("providing 2 input files and a single layout works", {
  test_files <- c("example_full.xls", "example_no_layout.xls")
  .df <- read_plate(test_files)
  expect_equal(unique(.df[["file"]]), test_files)
  expect_equal(nrow(.df), 96 * 2)
})

test_that("reading a single excel file with more than one layout generates a specific error", {
  expect_error(read_plate(c("example_full.xls", "example_layout.xls")),
               "^Only a single ID table is supported$")
})
