context("elisa.analyse")

test_that("the lack of an 'id' column generates an error", {
  expect_error(elisa.analyse(1), "^Missing mandatory column 'id'$")
  expect_error(elisa.analyse(data.frame(x = 1:10)), "^Missing mandatory column 'id'$")
})

test_that("performing the 4-PL regression without custom arguments generates the appropriate values", {
  read.plate("example_full.xls") %>%
    elisa.analyse() %>%
    summarise(od = sum(value), concentration = floor(sum(concentration))) %>%
    as.data.frame() %>%
    expect_equivalent(data.frame(od = 26.364, concentration = 12534))
})

test_that("performing the 4-PL regression with a log10 transformation generates the appropriate values", {
  .df <-  read.plate("example_full.xls") %>%
    elisa.analyse(transform = TRUE) %>%
    summarise(od = sum(value), concentration = floor(sum(concentration))) %>%
    as.data.frame() %>%
    expect_equivalent(data.frame(od = 26.364, concentration = 11692))
})



test_that("performing a common 4-PL regression for more than one file is possible", {
  .df <- read.plate(c("example_full.xls", "example_no_layout.xls"), layout = "example_layout.xls")
  model <- .df %>%
    elisa.analyse(multi.regression = FALSE) %>%
    attr("model")
  
  expect_equivalent(model %>% colnames, c("file", "Slope:(Intercept)", "Lower:(Intercept)", "Upper:(Intercept)", "ED50:(Intercept)"))
  expect_equal(model %>% nrow , 1)
})

test_that("setting a dilution factor column works", {
  .df <- read.plate("example_full.xls") %>%
    mutate(dilution = 2) %>%
    elisa.analyse(dilution = "dilution") %>%
    summarise(od = sum(value), concentration = floor(sum(concentration))) %>%
    as.data.frame() %>%
    expect_equivalent(data.frame(od = 26.364, concentration = 25068))
})


test_that("printing the data.frame generates an output starting with a specific character sequence", {
  .df <- read.plate("example_full.xls") %>%
    elisa.analyse()
  expect_output(print.elisa_df(.df), "elisa.analyse\\(\\) concentration values obtained from the OD with the following 4PL regression.*")
})

test_that("extracting the standard curve dataframe is working", {
  .df <- read.plate("example_full.xls") %>%
    elisa.standard()
  expect_equal(.df %>% colnames, c("column", "row", "id", "x", "value"))
})

test_that("extracting the standard curve dataframe and keeping additional columns is working", {
  .df <- read.plate("example_full.xls") %>%
    elisa.standard(.keep = c("file", "sheet"))
  expect_equal(.df %>% colnames, c("column", "row", "id", "x", "value", "file", "sheet"))
})

test_that("keeping a column that does not exist generates a specific error", {
  expect_error(read.plate("example_full.xls") %>%
                 elisa.standard(.keep = "test"), "^Cannot keep a column that does not exist$")
})
