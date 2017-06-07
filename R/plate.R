#' @import tidyverse
#' @import readxl
#' @import digest

# finds a 6, 12, 24 or 96 well plate in .df at the coordinates of "A"
find_plate <- function(.df, col, row, plate_sizes = c(6, 12, 24, 96)) {
  tibble(plate_size = c(6, 12, 24, 96), n_cols = c(3, 4, 6, 12), n_rows = c(2, 3, 4, 8)) %>%
    filter(plate_size %in% plate_sizes) %>%
    filter(nrow(.df) >= row + n_rows - 1, ncol(.df) >= col + n_cols - 1) %>%
    mutate(valid_row = map2_lgl(n_cols, n_rows, ~all(.df[row:(row + .y - 1), col] == LETTERS[1:.y])),
           valid_col = map2_lgl(n_cols, n_rows, ~all(.df[row - 1, (col + 1):(col + .x)] == 1:.x))) %>%
    filter(valid_row, valid_col) %>%
    filter(plate_size == max(plate_size)) %>%
    select(plate_size, n_cols, n_rows)
}

# After reading the excel sheet as a character data frame, we locate the "A" character
# Plates are defined with rows from A up to H and columns from "1" up to "12"
# The helper function find_plate will find out if a plate indentifier header matches the position of "A"

read_plate_single <- function(input, sheet = 1) {
  xl_sheet <- input %>%
    read_excel(sheet = sheet, col_names = FALSE, col_types = "text")
  
  which(xl_sheet == "A", arr.ind = TRUE) %>%
    as_tibble() %>%
    mutate(plate_size = map2(col, row, find_plate, .df = xl_sheet)) %>%
    unnest() %>%
    mutate(plate = pmap(list(col, row, n_cols, n_rows),
                        function(col, row, n_cols, n_rows) {
                          read_excel(input, range = cell_limits(c(row - 1, col + 1), c(row + n_rows - 1, col + n_cols)), sheet = sheet)
                        }
    ),
    is_numeric = at_depth(plate, 2, is.numeric),
    is_numeric = map(is_numeric, flatten_lgl),
    is_numeric = map_lgl(is_numeric, all)) %>%
    select(row, col, n_rows, n_cols, plate_size, is_numeric, plate)
}

#' @export
read_plate <- function(input) {
  if (!all(file.exists(input))) {
    # At least one file is missing:
    stop(glue::glue("Could not find the following file(s): {input[which(!file.exists(input))]}"))
  }
  
  .df <- input %>%
    enframe("name", "path") %>%
    mutate(name = replace(name, is.numeric(name) | name == "", basename(path[is.numeric(name) | name == ""]))) %>%
    distinct()
  
  if (!(.df %>% summarise(n_distinct(name) == n()) %>% flatten_lgl())) stop("Distinct files must be named differently")
  
  .df <- .df %>% 
    mutate(sheet = map(path, excel_sheets)) %>%
    unnest() %>%
    mutate(plates = map2(path, sheet, read_plate_single)) %>%
    unnest() %>%
    group_by(name, path, plate_size) %>%
    mutate(n_layout =  length(is_numeric[is_numeric == FALSE])) %>%
    ungroup()
  
  # For now we consider non numeric data frames as layouts
  # An excel sheet containing multiple layouts is not valid
  if (nrow(bad_file <- .df %>% filter(n_layout > 1))) warning(glue::glue("Multiple layouts are not supported: ignoring {bad_file %>% distinct(path)}"))
  .df
  .df %>% 
    filter(n_layout == 1) %>%
    mutate(plate = map2(plate, n_rows, ~mutate(.x, row = LETTERS[1:.y])),
           plate = map(plate, gather, col, value, -row)) %>%
    group_by(name, path, plate_size) %>%
    mutate(layout = plate[is_numeric == FALSE],
           layout = map(layout, rename, id = value)) %>% 
    filter(is_numeric) %>%
    mutate(plate = map2(plate, layout, inner_join, by = c("row", "col"))) %>%
    select(name, path, sheet, row, col, plate_size, plate) %>%
    unnest()
}