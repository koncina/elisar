context("Import plates (read.plate)")

example.full <- system.file("extdata", "example_full.xls", package="elisar")
example.layout <- system.file("extdata", "example_layout.xls", package="elisar")
example.no.layout <- system.file("extdata", "example_no_layout.xls", package="elisar")
example.not.working <- system.file("extdata", "datasets.xlsx", package="readxl")

test_that("Reading non existing will generate an error", {
  expect_error(read.plate(c("", "nofile")))
})

test_that("Reading NA or numericals will generate an error in file.exists", {
  expect_error(read.plate(c(NA, 1)))
  expect_error(read.plate(example.not.working))
})

test_that("elisa.analyse calculates the concentrations from the OD values", {
  expect_error(elisa.analyse(1))
})

test_that("Reading a valid excel file should return a dataframe", {
  expect_is(read.plate(example.full), "data.frame")
})

test_that("It is possible to specify the layout (external file or sheetname or sheet index)", {
  expect_is(read.plate(example.full, layout = "id"), "data.frame")
  expect_is(read.plate(example.full, layout = 2), "data.frame")
  expect_is(read.plate(example.no.layout, layout = example.layout), "data.frame")
  # Providing 2 input files and a single layout will duplicate the layout
  # In addition we will override an autodetected layout
  expect_is(read.plate(c(example.full, example.no.layout), layout = example.layout), "data.frame")
})

test_that("Reading a single excel file but providing 2 layouts should generate an error", {
  expect_error(read.plate(example.no.layout, layout = c(example.layout, example.layout)))
})

test_that("Providing a bad layout should generate an error", {
  expect_error(read.plate(example.no.layout, layout = 3))
})