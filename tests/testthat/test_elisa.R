context("elisa_analyse")

test_that("the lack of an 'id' column generates an error", {
  expect_error(elisa_analyse(1), "^\\.df is not a data.frame$")
  expect_error(elisa_analyse(data.frame(x = 1:10)), "^Missing column\\(s\\): value, id$")
})

test_that("performing the 4-PL regression without custom arguments generates the appropriate values", {
  read_plate("example_full.xls") %>%
    elisa_analyse() %>%
    summarise(od = sum(value), estimate = floor(sum(estimate))) %>%
    as.data.frame() %>%
    expect_equivalent(data.frame(od = 26.364, estimate = 12534))
})

test_that("performing the 4-PL regression with a log10 transformation generates the appropriate values", {
  c("example_full.xls") %>% 
    read_plate() %>%
    mutate(log_value = log10(value)) %>% 
    elisa_analyse(.od = log_value) %>%
    summarise(od = sum(value), estimate = floor(sum(estimate))) %>%
    expect_equivalent(tibble(od = 26.364, estimate = 11692))
})

test_that("performing a common 4-PL regression for more than one file is possible", {
  c("example_full.xls", "example_no_layout.xls") %>%
    read_plate() %>%
    elisa_analyse() %>%
    group_by(.group, file, element_id) %>%
    summarise(value = sum(value)) %>% 
    expect_equivalent(tibble(.group = 1L, file = c("example_full.xls", "example_no_layout.xls"), element_id = c(1L, 3L), value = 26.364))
})

test_that("setting a dilution factor column works", {
  c("example_full.xls") %>%
    read_plate() %>%
    mutate(dilution = 2) %>%
    elisa_analyse(dilution = dilution) %>%
    summarise(od = sum(value), estimate = floor(sum(estimate))) %>%
    expect_equivalent(tibble(od = 26.364, estimate = 25068))
})

test_that("extracting the standard curve dataframe is working", {
  c("example_full.xls") %>%
    read_plate() %>%
    get_standard() %>%
    colnames() %>%
    expect_equal(c("file", "col", "row", "id", ".dose", "value"))
})

test_that("extracting the standard curve dataframe without dropping additional columns is working", {
  read_plate("example_full.xls") %>%
    get_standard(.drop = FALSE) %>%
    colnames() %>%
    expect_equal(c("file", "col", "row", "id", "element_id", "sheet_name", "sheet_pos", "format", "description", "treatment", "medium", ".dose", "value"))
})
