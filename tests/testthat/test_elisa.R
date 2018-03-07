context("elisa_analyse")

test_that("performing the 4-PL regression without custom arguments generates the appropriate values", {
  od_data <- read_plate("example_full.xls", na = "Empty")
  od_data <- od_data[!is.na(od_data$id),]
  x <- suppressWarnings(elisa_analyse(od_data))
  x <- apply(x[,c("value", "estimate")], 2, sum)
  x["estimate"] <- floor(x["estimate"])
  expect_equivalent(x, c(value = 26.364, estimate = 12534))
})

test_that("performing the 4-PL regression and back the estimate as a vector works", {
  od_data <- read_plate("example_full.xls", na = "Empty")
  od_data <- od_data[!is.na(od_data$id),]
  x <- suppressWarnings(od_to_concentration(id = od_data[["id"]], value = od_data[["value"]]))
  expect_equivalent(length(x), nrow(od_data))
  x <- floor(sum(x))
  expect_equivalent(x, 12534)
})

test_that("standard can be extracted", {
  od_data <- read_plate("example_full.xls", na = "Empty")
  std_data <- extract_standard(od_data)
  expect_is(std_data, "data.frame")
  expect_equal(nrow(std_data), 14)
  expect_equal(sort(unique(std_data$x)), c(15.625, 31.25, 62.5, 125, 250, 500, 1000))
})

test_that("missing columns are detected", {
  od_data <- read_plate("example_full.xls", na = "Empty")
  expect_error(extract_standard(od_data, var_in = "another_input"),
               ".data must be a dataframe and contain at least the columns: id; another_input")
  expect_error(elisa_analyse(od_data, var_in = "another_input"),
               ".data must be a dataframe and contain at least the columns: id; another_input")
})

test_that("extracting standard with a different input variable works", {
  od_data <- read_plate("example_full.xls", na = "Empty")
  colnames(od_data)[colnames(od_data) == "value"] <- "another_input"
  std_data <- extract_standard(od_data, var_in = "another_input")
  expect_is(std_data, "data.frame")
  expect_equal(nrow(std_data), 14)
  expect_equal(sort(unique(std_data$x)), c(15.625, 31.25, 62.5, 125, 250, 500, 1000))
})

test_that("extracting standard generates a data frame with the right column names", {
  od_data <- read_plate("example_full.xls", na = "Empty")
  std_data <- extract_standard(od_data)
  expect_equal(colnames(std_data), c("x", "y"))
  std_data <- extract_standard(od_data, alt_x, alt_y)
  expect_equal(colnames(std_data), c("alt_x", "alt_y"))
  std_data <- extract_standard(od_data, "alt_x", "alt_y")
  expect_equal(colnames(std_data), c("alt_x", "alt_y"))
})