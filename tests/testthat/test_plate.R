context("read.plate")

test_that("reading non existing files generates a specific error message", {
  expect_error(read.plate(c("", "nofile")), "^Could not find the following file\\(s\\): , nofile$")
})

test_that("reading NA or numericals generates an error in file.exists", {
  expect_error(read.plate(c(NA, 1)), "^invalid 'file' argument$")
})

test_that("the workaround for a buggy supplied excel file works", {
  expect_message(read.plate("example_full.xls"), "readxl returned a dataframe without column names: Trying a workaround")
})

test_that("reading a valid excel file returns the appropriate dataframe", {
  .df <- read.plate("example_full.xls")
  expect_is(.df, "data.frame")
  expect_equal(colnames(.df), c("file", "row", "column", "id", "description", "treatment", "medium", "sheet", "value"))
  cs <- digest::digest(.df, algo = "sha1")
  expect_equal(cs, "1fbf4f77c89581ca36f70fea4f6bd295c7a42628")
})

test_that("It is possible to override the layout (external file or sheetname or sheet index)", {
  expect_message(.df2 <- read.plate("example_full.xls", layout = "id"), "Forcing layout to sheet id in .*example_full.xls")
  expect_message(.df3 <- read.plate("example_full.xls", layout = 2), "Forcing layout to sheet 2 in .*example_full.xls")
  expect_message(.df4 <- read.plate("example_full.xls", layout = "example_layout.xls"), "Overriding detected layout in .*example_full.xls")
  
  .df <- read.plate("example_full.xls")
  
  expect_equal(.df, .df2)
  expect_equal(.df, .df3)
  expect_equal(.df, .df4)

})

test_that("providing 2 input files and a single layout will duplicate the layout", {
  .df <- read.plate(c("example_full.xls", "example_no_layout.xls"), layout = "example_layout.xls") %>%
    group_by(file) %>%
    summarise(value = sum(value)) %>%
    as.data.frame() %>%
    expect_equivalent(data.frame(file = c("example_full.xls", "example_no_layout.xls"), value = c(26.364, 26.364), stringsAsFactors = FALSE))
})

test_that("reading a single excel file with more than one layout generates a specific error", {
  expect_error(read.plate("example_no_layout.xls", layout = c("example_layout.xls", "example_layout.xls")),
               "^layout argument should have a length of 1 or equal to input!$")
})

test_that("providing a bad layout generates a specific error", {
  expect_error(read.plate("example_no_layout.xls", layout = 3), "^Bad layout argument$")
})