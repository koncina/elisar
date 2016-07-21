context("analysing plots")

example.full <- system.file("extdata", "example_full.xls", package="elisar")
example.df <- elisar::read.plate(example.full)

test_that("Using the stat_4pl layer should render a plot", {
  library(dplyr)
  library(ggplot2)
  library(scales)
  example.df %>%
    elisa.standard() %>%
    ggplot(aes(x = x, y = value)) +
    geom_point() +
    stat_4pl() -> plot.test
  expect_is(plot.test, "ggplot")
})

test_that("Setting stat_4pl verbose option should still render a plot", {
  library(dplyr)
  library(ggplot2)
  library(scales)
  example.df %>%
    elisa.standard() %>%
    ggplot(aes(x = x, y = value)) +
    geom_point() +
    stat_4pl(verbose = TRUE) -> plot.test
  expect_is(plot.test, "ggplot")
})

test_that("Setting stat_4pl logDose option should still render a plot", {
  library(dplyr)
  library(ggplot2)
  library(scales)
  example.df %>%
    elisa.standard() %>%
    ggplot(aes(x = x, y = value)) +
    geom_point() +
    stat_4pl(logDose = 10) -> plot.test
  expect_is(plot.test, "ggplot")
})

test_that("Setting stat_4pl logDose option should still render a plot", {
  library(dplyr)
  library(ggplot2)
  library(scales)
  example.df %>%
    elisa.standard() %>%
    ggplot(aes(x = x, y = value)) +
    scale_x_log10() +
    geom_point() +
    stat_4pl() -> plot.test
  expect_is(plot.test, "ggplot")
})

test_that("Setting stat_4pl options should still render a plot", {
  library(dplyr)
  library(ggplot2)
  library(scales)
  example.df %>%
    elisa.standard() %>%
    ggplot(aes(x = x, y = value)) +
    geom_point() +
    stat_4pl(verbose = TRUE, logDose = 10) -> plot.test
  expect_is(plot.test, "ggplot")
})


