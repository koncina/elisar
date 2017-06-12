#' @import dplyr
#' @import tidyr
#' @importFrom purrr map_lgl map_int map_df map
#' @import tibble
#' @import readxl
#' @import digest

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
    mutate(range = pmap(list(col, row, n_col, n_row), function(x, y, n_x, n_y) cell_limits(c(y, x), c(y + n_y, x + n_x))))
}


find_id <- function(.df) {
  empty_row <- .df %>%
    is.na() %>%
    t() %>%
    as_tibble() %>%
    map(all) %>%
    flatten_lgl() %>% 
    set_names(seq_along(.))
  
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

# Extract all elements (plates and id tables) from a single excel file

find_elements <- function(path) {
  path %>%
    excel_sheets() %>%
    set_names(glue::glue("{seq_along(.)}_{.}")) %>%
    map(read_excel, path = path, col_names = FALSE, col_types = "text", range = cell_limits(ul = c(1, 1))) %>%
    enframe("sheet", "data") %>%
    separate(sheet, c("sheet_pos", "sheet_name"), sep = "_", extra = "merge") %>%
    mutate(path = path,
           plate = map(data, find_plate),
           id = map(data, find_id))
}

join_if_id <- function(.x, .y, .by) {
  if (is.null(.x) || length(.x) == 0) return(.y)
  .x %>%
    mutate_at(vars(id), as.character) %>%
    right_join(.y, by = .by)
}

#' @export
read_plate <- function(path) {
  associated_id <- quo(element == "id" & lag(element) == "plate")
  path %>%
    map_df(find_elements) %>%
    gather(element, where, plate, id) %>%
    unnest(where) %>%
    arrange(path, sheet_pos, row) %>%
    rownames_to_column("element_id") %>%
    group_by(path, sheet_pos) %>%
    mutate(element_id = replace(element_id, !!associated_id, lag(element_id)[!!associated_id])) %>%
    ungroup() %>%
    mutate(element = forcats::as_factor(element),
           element = forcats::fct_expand(element, c("plate", "id")),
           data = pmap(list(path, sheet_name, range), read_excel)) %>%
    select(element_id:element, data) %>%
    spread(element, data, drop = FALSE) %>%
    filter(map_int(plate, length) > 0) %>% # Remove orphan id tables
    mutate(plate = map(plate, gather, col, value, -1),
           plate = map(plate, set_names, c("row", "col", "value")),
           plate = map2(id, plate, join_if_id, c("id" = "value")),
           id = map_int(id, length),
           id = map_lgl(id, as.logical),
           format = map_int(plate, nrow),
           path = basename(path)) %>%
    select(file = path, sheet_pos, sheet_name, element_id, format, id, data = plate)
}

#' @export
join_id <- function(.data) {
  n_id <- .data %>%
    group_by(file, format) %>%
    filter(id) %>%
    summarise(n = n()) %>%
    select(file, n) %>%
    deframe()
  
  if (any(n_id > 1)) stop("Cannot join multiple identifiers...")
  
  .data %>%
    group_by(file, format) %>%
    mutate(data_id = replace(list(NULL), length(data[id]) > 0, data[id])) %>%
    filter(!id) %>%
    mutate(data = map2(data_id, data, join_if_id, c("row", "col"))) %>%
    select(-data_id, -id) %>%
    ungroup()
}
