NULL

# read the spreadsheet as a character matrix to identify elements
read_xls_matrix <- function(path, sheet = 1, na = "") {
  xls_matrix <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", na = na)
  xls_matrix <- unname(as.matrix(xls_matrix))
  attr(xls_matrix, "sheet") <- sheet
  attr(xls_matrix, "path") <- path
  xls_matrix
}

set_element_attributes <- function(x, xls_matrix_attr, type, col_start, col_end, row_start, row_end) {
  attr(x, "coordinates") <- c(col_start = col_start, col_end = col_end, row_start = row_start, row_end = row_end)
  attr(x, "type") <- type
  attr(x, "origin") <- list(file = xls_matrix_attr[["path"]],
                            sheet = xls_matrix_attr[["sheet"]])
  x
}

debug_line <- function(elt_attribute) {
  elt_attribute <- with(elt_attribute, c(type = type, origin[c("file", "sheet")], coordinates[c("col_start", "row_start", "col_end", "row_end")]))
  with(elt_attribute, message(sprintf("... Detected '%s' element at (col: %s, row: %s, width: %s, height: %s) in '%s' on sheet '%s'", type, col_start, row_start, col_end - col_start, row_end - row_start, basename(file), sheet)))
}

# Display some informations on detected elements
error_message <- function(elements_list, msg) {
  lapply(elements_list, function(x) debug_line(attributes(x)))
  stop(msg, call. = FALSE)
}

# In the xls sheet matrix try to detect keywords for ID table or plate ("id" or row header "A")
# For both functions we use apply to make a list out of the matrix ans switch to lapply: will make our life easier
find_keyword <- function(xls_matrix, keyword) {
  if (!keyword %in% c("A", "id")) stop("Unrecognized keyword", call. = FALSE)
  coord_finder <- list(id = find_coordinates_id, A = find_coordinates_plate)[[keyword]]
  keyword_list <- apply(which(xls_matrix == keyword, arr.ind = TRUE, useNames = FALSE), 1, as.list)
  keyword_list <- lapply(keyword_list, function(x) coord_finder(x[[1]], x[[2]], xls_matrix))
  remove_empty(keyword_list)
}

find_coordinates_id <- function(row_start, col_start, xls_matrix) {
  empty_rows <- which(rowSums(`!`(is.na(xls_matrix))) == 0)
  
  line <- xls_matrix[row_start,]
  
  col_start <- which(!is.na(line))[1]
  
  line_rle <- rle(!is.na(line))
  
  header_block <- which(line_rle[["values"]])
  if (length(header_block) > 1) warning("skipped ID table: splitted headers are not supported")
  
  col_end <- col_start + line_rle[["lengths"]][header_block] - 1
  
  row_end <- empty_rows[empty_rows > row_start][1] - 1
  if (is.na(row_end)) row_end <- nrow(xls_matrix)
  
  c(col_start = col_start, col_end = col_end, row_start = row_start, row_end = row_end)
}


find_coordinates_plate <- function(row_start, col_start, xls_matrix) {
  # We need to adjust the row: keyword "A" is one row below
  row_start <- row_start - 1
  row_names <- xls_matrix[-(1:row_start), col_start]
  max_row_length <- min(8, length(row_names))
  row_names <- row_names[1:max_row_length]
  
  col_names <- xls_matrix[row_start, -(1:col_start)]
  max_col_length <- min(12, length(col_names))
  col_names <- col_names[1:max_col_length]
  
  # Check against the reference headers using rle
  row_names <- rle(row_names == LETTERS[1:max_row_length])
  col_names <- rle(col_names == as.character(1:max_col_length))
  
  if (!all(row_names[["values"]][1], col_names[["values"]][1])) return(NULL)
  
  # Define valid plaste sizes for 6, 12, 24, 48 and 96 well plates
  valid_cols <- c(3, 4, 6, 8, 12)
  valid_rows <- c(2, 3, 4, 6, 8)
  
  # Find the best match using the detected plate headers
  best_match <- max(which((valid_cols <= col_names[["lengths"]][1] & valid_rows <= row_names[["lengths"]][1])))
  
  c(col_start = col_start, col_end = col_start + valid_cols[best_match], row_start = row_start, row_end = row_start + valid_rows[best_match])
}

extract_id_table <- function(col_start, col_end, row_start, row_end, xls_matrix) {
  m <- xls_matrix[(row_start + 1):row_end, col_start:col_end]
  colnames(m) <- xls_matrix[row_start, col_start:col_end]
  m <- as.data.frame(m, stringsAsFactors = FALSE)
  m[] <- lapply(m, type.convert, as.is = TRUE)
  m <- set_element_attributes(m, attributes(xls_matrix), "id", col_start, col_end, row_start, row_end)
  m
}

extract_plate <- function(col_start, col_end, row_start, row_end, xls_matrix) {
  m <- xls_matrix[(row_start + 1):row_end, (col_start + 1):col_end]
  m <- type.convert(m, as.is = TRUE)
  colnames(m) <- 1:(col_end - col_start)
  rownames(m) <- LETTERS[1:(row_end - row_start)]
  m <- set_element_attributes(m, attributes(xls_matrix), ifelse(is.numeric(m), "data", "layout"), col_start, col_end, row_start, row_end)
  m
}

get_element_list <- function(xls_matrix, coordinates, extractor) {
  lapply(coordinates, function(x) do.call(extractor, c(x, xls_matrix = substitute(xls_matrix))))
}

#' Extract elements from excel files 
#'
#' Extract OD data, layout and ID tables from an excel file. Returns a list containing the plates as a matrix and the ID tables as `data.frame`.
#'
#' @param path vector containing the path(s) to the input file(s)
#' 
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet) (See `readxl::read_excel` help page).
#'
#' @param na Character vector of strings to use for missing values. (See `readxl::read_excel` help page)
#'
#' @details Example on how to prepare the excel file can be found at \url{https://github.com/koncina/elisar}.
#'
#' @examples
#' \dontrun{
#' library(elisar)
#'
#' # Import file(s)
#' example_file <- system.file("extdata", "example.xls", package="elisar")
#' extract_elements(example_file, 1)
#' extract_elements(example_file, 2)
#' }
#'
#' @export
extract_elements <- function(path, sheet = 1, na = "") {
  xls_matrix <- read_xls_matrix(path, sheet, na)
  c(get_element_list(xls_matrix, find_keyword(xls_matrix, "A"), "extract_plate"),
    get_element_list(xls_matrix, find_keyword(xls_matrix, "id"), "extract_id_table"))
}
