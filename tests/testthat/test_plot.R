context("plot")

test_that("using the stat_4pl layer renders a plot", {
  library(ggplot2)
  library(scales)
  od_data <- read_plate("example_full.xls", na = "Empty")
  std <- extract_standard(od_data)
  ggplot(std, aes(x = x, y = y)) +
    geom_point() +
    stat_4pl() -> plot.test
  expect_is(plot.test, "ggplot")
})

test_that("setting the stat_4pl logDose option still renders a plot", {
  library(ggplot2)
  library(scales)
  od_data <- read_plate("example_full.xls", na = "Empty")
  std <- extract_standard(od_data)
  p <- ggplot(std, aes(x = x, y = value)) +
    geom_point()
  expect_is(p + stat_4pl(logDose = 10), "ggplot")
})

