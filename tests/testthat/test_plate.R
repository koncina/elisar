context("read_plate")

test_that("reading non existing files generates a specific error message", {
  expect_error(read_plate("nofile"), "^'nofile' does not exist in current working directory")
})

test_that("reading NA or numericals generates an error in file.exists", {
  expect_error(read_plate(c(NA, 1)), "^invalid 'file' argument$")
})

test_that("reading a valid excel file returns the appropriate dataframe", {
  .df <- read_plate("example_full.xls")
  expect_is(.df, "data.frame")
  expect_equal(colnames(.df), c("element_id", "file", "sheet_name", "sheet_pos", "format", "row", "col", "id", "description", "treatment", "medium", "value"))
  cs <- digest::digest(.df, algo = "sha1")
  expect_equal(cs, "d370e8f7b464c0368742b81e6a6bdf10139a2298")
})
