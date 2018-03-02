build_df <- function(data_v, layout_v) {
  elt_attr <- attributes(data_v)
  dimension <- elt_attr[["dim"]]
  data.frame(
    file = basename(elt_attr[["origin"]][["file"]]),
    sheet = elt_attr[["origin"]][["sheet"]],
    element = do.call(sprintf, as.list(c("(%s, %s)", elt_attr[["coordinates"]][c("row_start", "col_start")]))),
    row = rep(LETTERS[1:dimension[1]], dimension[2]),
             col = rep(1:dimension[2], each = dimension[1]),
             id = layout_v,
             value = as.vector(data_v),
    stringsAsFactors = FALSE)
}

#' Import the OD/luminescence/fluorescence measures from plate reader exported excel sheets (like Fluostar Optima, or Tecan Sunrise)
#'
#' Reads excel files exported from the MARS software (\url{http://www.bmglabtech.com/en/products/software/mars-data-analysis/}) or Tecan Magellan (\url{http://lifesciences.tecan.com/products/software/magellan_data_analysis_software}) and modified to include the plate layout (see documentation).
#'
#' @param input vector containing the path(s) to the input file(s)
#' 
#' @details Example on how to prepare the excel file can be found at \url{https://github.com/koncina/elisar}.
#'
#' @examples
#' \dontrun{
#' library(elisar)
#'
#' # Import file(s)
#' df <- read_plate("od_measure.xls")
#' df <- read_plate(c("od_measure1.xls", "od_measure2.xls"))
#' }
#'
#' @export
read_plate <- function(path, na = "") {
  elements_list <- lapply(path, function(x) lapply(readxl::excel_sheets(x), extract_elements, path = x, na = na))
  elements_list <- unlist_recursive(elements_list, depth = 2)
  # For now we will support only a single layout and ID table for all files (might change in a future release)
  # Extracting the type of each elements
  element_type <- sapply(elements_list, function(x) attributes(x)[["type"]])
  id_element <- which(element_type == "id")
  layout_element <- which(element_type == "layout")
  if (length(id_element) > 1) error_message(elements_list, "Only a single ID table is supported")
  if (length(layout_element) > 1) error_message(elements_list, "Only a single layout plate is supported")
  
  element_dim <- lapply(elements_list, function(x) attributes(x)[["dim"]])
  element_dim <- unique(remove_empty(element_dim))
  
  if (length(element_dim) != 1) error_message(elements_list, "Plates must have same dimensions")
  # Extract layout vector
  layout_v <- as.vector(elements_list[[layout_element]])

  elt_df <- do.call(rbind, lapply(elements_list[-c(layout_element, id_element)], build_df, layout_v = layout_v))
  
  if (length(id_element) == 1) elt_df <- merge(elt_df, elements_list[[id_element]],
                                               by = "id", sort = FALSE, all.x = TRUE)[, union(names(elt_df), names(elements_list[[id_element]]))]
  class(elt_df) <- c("tbl_df", "tbl", "data.frame") # Setting tibble class to allow pretty printing without tibble dependency
  elt_df
}
