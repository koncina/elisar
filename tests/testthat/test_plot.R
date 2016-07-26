context("plot")

test_that("using the stat_4pl layer renders a plot", {
  library(dplyr)
  library(ggplot2)
  library(scales)
  read.plate("example_full.xls") %>%
    elisa.standard() %>%
    ggplot(aes(x = x, y = value)) +
    geom_point() +
    stat_4pl() -> plot.test
  expect_is(plot.test, "ggplot")
})

test_that("setting stat_4pl to verbose renders a plot and a message", {
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  p <- read.plate("example_full.xls") %>%
    elisa.standard() %>%
    ggplot(aes(x = x, y = value)) +
    geom_point()
  
  expect_message(p + stat_4pl(verbose = TRUE), "^Showing 4PL model details")
  expect_is(p + stat_4pl(verbose = TRUE), "ggplot")
})

test_that("setting the stat_4pl logDose option still renders a plot", {
  library(dplyr)
  library(ggplot2)
  library(scales)
  p <- read.plate("example_full.xls") %>%
    elisa.standard() %>%
    ggplot(aes(x = x, y = value)) +
    geom_point()
  expect_is(p + stat_4pl(logDose = 10), "ggplot")
})

