context("analysing elisa")

example.full <- system.file("extdata", "example_full.xls", package="elisar")
example.layout <- system.file("extdata", "example_layout.xls", package="elisar")
example.no.layout <- system.file("extdata", "example_no_layout.xls", package="elisar")
example.df <- elisar::read.plate(c(example.full, example.no.layout), layout = c(NA, example.layout))

test_that("Performing the 4-PL regression should return a dataframe", {
  expect_is(elisa.analyse(example.df), "data.frame")
  expect_is(elisa.analyse(example.df, transform = TRUE), "data.frame")
  expect_is(elisa.analyse(example.df, multi.regression = FALSE), "data.frame")
  example.df$dilution <- 2
  expect_is(elisa.analyse(example.df, dilution = "dilution"), "data.frame")
})

test_that("Printing the data.frame", {
  expect_is(print(elisa.analyse(example.df)), "character")
})

test_that("Providing an invalid dataframe should generate an error", {
  expect_error(elisa.analyse(data.frame(x = 1:10)))
})

test_that("Extracting the standard curve datapoints should return a dataframe", {
  expect_is(elisa.standard(example.df), "data.frame")
})

