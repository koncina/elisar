context("elisa_analyse")

test_that("performing the 4-PL regression without custom arguments generates the appropriate values", {
  x <- read_plate("example_full.xls", na = "Empty")
  x <- suppressWarnings(elisa_analyse(x))
  x <- apply(x[,c("value", "estimate")], 2, sum)
  x["estimate"] <- floor(x["estimate"])
  expect_equivalent(x, c(value = 26.364, estimate = 12534))
})

test_that("performing the 4-PL regression and back the estimate as a vector works", {
  od_data <- read_plate("example_full.xls", na = "Empty")
  x <- suppressWarnings(get_concentration(id = od_data[["id"]], value = od_data[["value"]]))
  expect_equivalent(length(x), nrow(od_data))
  x <- floor(sum(x))
  expect_equivalent(x, 12534)
})

