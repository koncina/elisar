#' @import dplyr
#' @import tidyr
#' @importFrom purrr map_lgl map_int map_df map
#' @import tibble
#' @import readxl
#' @import digest

# Helper function to find the the plates (6, 12, 24, 48 or 96 well plates)
# Plates are defined as having column and row headers
# Columns are named from 1 up to 12
# Rows are named from A up to H
find_plate <- function(.df) {
  valid_plate <- tribble(~format, ~n_row, ~n_col,
                         6, 2, 3,
                         12, 3, 4,
                         24, 4, 6,
                         48, 6, 8,
                         96, 8, 12)
  
  max_col <- max(valid_plate[["n_col"]])
  max_row <- max(valid_plate[["n_row"]])
  
  c(row = LETTERS[1:max_row], col = 1:max_col)
  
  .t <- which(.df == "A", arr.ind = TRUE) %>%
    as_tibble() %>%
    mutate(row = row - 1,
           header_row = map2(col, row, ~.df[(.y + 1):(.y + max_row), .x]),
           header_col = map2(col, row, ~.df[.y, (.x + 1):(min(.x + max_col, ncol(.df)))])) %>%
    mutate_at(vars(header_row:header_col), map, flatten_chr) %>%
    gather(header_type, header, starts_with("header")) %>%
    mutate(header_type = gsub("header_", "", header_type),
           t = recode(header_type, "row" = list(LETTERS[1:max_row]), "col" = list(1:max_col)),
           t = map2(header, t, quietly(`==`)),
           t = map(t, "result"),
           t = map(t, rle),
           t = map(t, transpose),
           t = map(t, bind_rows),
           t = map(t, `[`, 1, TRUE))
  
  if (nrow(.t) == 0) return(tibble(row = numeric(0), col = numeric(0)))
  
  .t %>%
    unnest(t) %>%
    filter(values) %>%
    select(-header, -values) %>%
    rename(header_n = header_type) %>%
    spread(header_n, lengths, sep = "_") %>%
    mutate(plate = map2(header_n_row, header_n_col, ~ valid_plate %>% filter(n_row <= .x, n_col <= .y) %>% top_n(n = 1, format))) %>%
    unnest(plate) %>%
    select(-starts_with("header")) %>%
    mutate(is_numeric = pmap(list(col, row, n_col, n_row), function(x, y, n_x, n_y) .df[(y + 1):(y + n_y), (x + 1):(x + n_x)]),
           is_numeric = at_depth(is_numeric, 2, guess_parser),
           is_numeric = map(is_numeric, `%in%`, c("double", "integer")),
           is_numeric = map_lgl(is_numeric, all),
           range = pmap(list(col, row, n_col, n_row), function(x, y, n_x, n_y) cell_limits(c(y, x), c(y + n_y, x + n_x))))
}

# Helper function to find the localisation of ID tables (extended information for the layout plates)
# These tables must contain a header containing at least the column "id" which refering to the layout plate cells.
find_id <- function(.df) {
  empty_row <- .df %>%
    is.na() %>%
    t() %>%
    as_tibble() %>%
    map(all) %>%
    flatten_lgl() %>% 
    set_names(as.character(seq_along(.))) # dev version of purrr does not coerce to character anymore...
  
  .t <- which(.df == "id", arr.ind = TRUE) %>%
    as_tibble() %>%
    filter(c(TRUE, empty_row)[row]) %>% # previous row should be empty
    mutate(t = map(row, ~empty_row[.:length(empty_row)]),
           t = map(t, rle),
           t = map(t, transpose),
           t = map(t, bind_rows),
           t = map(t, `[`, 1, TRUE))
  
  if (nrow(.t) == 0) return(tibble(row = numeric(0), col = numeric(0)))
  
  .t %>% 
    unnest(t) %>%
    filter(!values) %>%
    select(row, n_row = lengths) %>% # the number of non empty lines is the length of our table
    mutate(range = map2(row, n_row, ~cell_rows(c(.x, .x + .y))))
}

#' List plate elements in excel files
#'
#' List all plate (data and layout) and associated extended id tables.
#' 
#' @param path vector containing the path to the excel files.
#' 
#' @return A tibble listing the found elements: element (plate or id), position (row, col), format (6, 12, 24, 48, 96 well plate), range (cellranger::cell_limits S3 object to be used in read_excel)
#'
#' @export
list_elements <- function(path, na = "") {
  associated_id <- quo(element == "id" & lag(element) == "layout")
  path %>%
    unique() %>%
    set_names() %>%
    map(excel_sheets) %>%
    enframe("path", "sheet_name") %>%
    unnest() %>%
    group_by(path) %>%
    mutate(sheet_pos = seq_along(sheet_name),
           data = map2(path, sheet_name, read_excel, col_names = FALSE, col_types = "text", range = cell_limits(ul = c(1, 1)), na = na),
           id = map(data, find_id),
           data = map(data, find_plate)) %>%
    ungroup() %>%
    gather(element, where, data, id) %>%
    unnest() %>%
    arrange(path, sheet_pos, row) %>%
    group_by(path, sheet_name) %>% # We won't accept a layout on one sheet and the extended id on the next one.
    mutate(element = replace(element, element == "data" & lead(element) == "id", "layout"),
           element = replace(element, !is_numeric, "layout")) %>%
    ungroup() %>%
    mutate(element_id = replace(NA, element != "id", seq_along(element[element != "id"]))) %>%
    group_by(sheet_pos) %>%
    mutate(element_id = walk(replace(element_id, !!associated_id, lag(element_id)[!!associated_id]),
                             ~if (is.na(.)) warning("Found orphan ID table", call. = FALSE))) %>%
    ungroup() %>%
    select(element_id, everything(), -is_numeric, -n_row, -n_col)
}

#' Read plate elements in excel files
#'
#' Read all plate (data and layout) and associated extended id tables.
#' 
#' @param .df tibble generated by list_elements().
#' 
#' @return A nested tibble containing the data elements: is_layout (boolean), format (6, 12, 24, 48, 96 well plate), data (nested tibble containing the tidy data)
#'
#' @export
read_elements <- function(.df, na = "") {
  .df %>%
    filter(!is.na(element_id)) %>%
    mutate(element = forcats::as_factor(element),
           element = forcats::fct_expand(element, c("data", "layout", "id")), # to generate all columns in spread 
           data = pmap(list(path, sheet_name, range, na = na), read_excel)) %>%
    select(element_id:element, data) %>%
    mutate(data = map_if(data, element %in% c("layout", "data"), gather, key, value, -1 ),
           data = map_if(data, element == "data", set_names, c("row", "col", "value")),
           data = map_if(data, element == "layout", set_names, c("row", "col", "id"))) %>% 
    spread(element, data, drop = FALSE) %>% 
    mutate(layout = map2(layout, id, ~if (is_empty(.y)) {.x} else {left_join(.x, .y, by = "id")})) %>%
    select(-id) %>%
    gather(element, data, layout, data) %>%
    filter(!map_dbl(data, is_empty)) %>%
    mutate(format = map_int(data, nrow),
           path = basename(path)) %>%
    arrange(element_id, path, sheet_pos) %>%
    rename(file = path) %>%
    select(everything(), -data, data)
}

#' @export
join_layout <- function(.df) {
  
  join_if_present <- function(data_id, data, file, element_id) {
    if (is_empty(data_id)) {
      warning(glue::glue("No layout found for element {element_id} in {file}"), call. = FALSE);
      data
    } else {
      right_join(data_id, data, by = c("row", "col"))
    }
  }
  
  # backup original groups
  g <- .df %>%
    groups()
  
  n_id <- .df %>%
    group_by(format, add = TRUE) %>%
    bind_cols(.group = group_indices(.)) %>%
    group_by(.group) %>%
    filter(element == "layout") %>%
    summarise(n = n()) %>%
    select(.group, n) %>%
    deframe()
  
  if (any(n_id > 1)) stop("Cannot join multiple layouts in each group...", call. = FALSE)
  .df %>%
    group_by(format, add = TRUE) %>%
    mutate(data_id = replace(list(NULL), length(data[element == "layout"]) > 0, data[element == "layout"])) %>%
    filter(element == "data") %>%
    mutate(data = pmap(list(data_id, data, file, element_id), join_if_present)) %>%
    select(-data_id, -element) %>%
    group_by(!!!g) # restore groups
}

#' @export
read_plate <- function(path, na = "") {
  path %>%
    list_elements(na = na) %>%
    read_elements(na = na) %>%
    group_by(file) %>%
    join_layout() %>%
    unnest() %>%
    select(-value, value)
}

