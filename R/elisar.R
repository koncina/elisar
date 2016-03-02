#' @import readr readxl
#' @import dplyr
#' @importFrom tidyr gather
#' @import broom

mutate_if <- function(.data, is.true = TRUE, ...) {
  if (isTRUE(is.true)) return(mutate(.data, ...))
  else return(.data)
}

fix.dataframe = function(.df, text = FALSE) {
  # Fixing the conversion of excel integers into string values generating trailing zeros (1 -> 1.000000)
  # Column names are fixed with readr::parse_number
  names(.df)[-1] <- parse_number(names(.df)[-1])
  names(.df)[names(.df) == '<>'] <- 'row'
  # If text is TRUE, we need to correct all converted values within the dataframe
  # We could use readr::parse_number but the regex approach might be safer as it will only change
  # string values ending with a dot followed by 6 zero characters.
  if (isTRUE(text)) .df <- tbl_df(as.data.frame(sub("\\.0{6}$", "", as.matrix(.df)), stringsAsFactors = FALSE))
  return(.df)
}

# Reading xls files with readxl might fail importing some text cells
# which would be replaced by "0.00" values...
is.readxl.bugging = function(.df) {
  check = function(x) {
    if ("0.00" %in% x) return(TRUE)
    return(FALSE)
  }
  b <- .df %>% summarise_each(funs(check)) %>% rowSums(.)
  if (b > 0) return(TRUE)
  return(FALSE)
}

#' Import the OD measures from the Tecan sunrise excel sheet
#'
#' Reads excel files exported from Tecan Sunrise and modified according to the documentation.
#'
#' @param input vector containing the path(s) to the input file(s)
#'
#' @details Example on how to prepare the Tecan excel sheet to be imported can be found at \url{http://eric.koncina.eu/r/elisar}.
#'
#' @examples
#' \dontrun{
#' library(elisar)
#'
#' # Import file
#' df <- elisa.load("od_measure.xls")
#' df <- elisa.load(c("od_measure1.xls", "od_measure2.xls"))
#' }
#'
#' @export
elisa.load = function(input) {
  # Switching from do.call to dplyr (thank to A. Ginolhac)
  lapply(input, elisa.load.single) %>% bind_rows()
}

# elisa.load.single will load a single input file
elisa.load.single = function(input) {
  # Function is expecting a Magellan xls with two additional sheets containing the plate layout and the sample IDs

  # Checking the extension
  ext <- gsub(".*\\.([[:alnum:]]+)$", "\\1", input)
  if (!ext %in% c("xls", "xlsx")) stop("Wrong input file format! (expecting an xls or xlsx Excel file)")

  data <- fix.dataframe(read_excel(input, sheet = 1)[1:8, 1:13]) %>%
    mutate_each(funs(as.numeric), -row) # Necessary if someone added some text outside the [1:8, 1:13] area
  layout <- fix.dataframe(read_excel(input, sheet = 2)[1:8, 1:13], text = TRUE)
  # Layout and id can reside on two different sheets (preferred)
  # We are checking whether a third sheet exists
  id <- tryCatch(read_excel(input, sheet = 3), error = function(e) return(NULL))
  if (is.null(id)) {
    # id are not present on the third sheet
    # Trying to locate the id table on the second sheet:
    id <- read_excel(input, sheet = 2, skip = 9, col_names = F)
    na.row <- apply(id, 1, function(x) all(is.na(x)))
    id <- read_excel(input, sheet = 2, skip = 8 + which(!na.row)[1])
  }
  
  # Readxl inaccurately imports empty columns (after an insert/remove cycle?)
  # We are excluding empty column names as dplyr will stop working
  id <- id[, colnames(id) != ""]
  # Alternative: remove completely empty columns:
  # http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
  # id <- id[, colSums(is.na(id)) < nrow(id)]
  
  if (!"id" %in% colnames(id)) stop("Missing column 'id'")
  # Fixing the number to string conversion (a readxl 'strict' behaviour)
  id$id <- sub("\\.0{6}$", "", id$id)
  data <- gather(data, key = column, value = od,  -row)
  layout  <- gather(layout, key = column, value = id,  -row)
  data <- full_join(layout, data, by = c("row", "column"))
  data <- full_join(data, id, by = c("id")) %>%
    filter(tolower(id) != "empty") %>%
    mutate(file = basename(input)) %>%
    select(file, row, column, id, od, everything())
  if (ext == "xls" && is.readxl.bugging(data)) message("Detected suspicious text values during import. Consider converting xls to xlsx!")
  return(data)
}

#' Draws the standard curve of the elisa analysis
#'
#' Requires ggplot2
#'
#' @param standard list object (or dataframe) returned by the elisa.analyze function
#'
#' @param concentration unit of the standard points (defaults to pg/ml)
#'
#' @details A complete example on how to perform an analysis can be found at \url{http://eric.koncina.eu/r/elisar}.
#'
#' @examples
#' \dontrun{
#' library(elisar)
#' library(ggplot2)
#'
#' # Import file
#' e <- elisa.load("od_measure.xls")
#' e <- elisa.analyze(e)
#' elisa.standard(e)
#' # OR supplying the dataframe
#' elisa.standard(e$standard)
#' }
#'
#' @export
elisa.standard = function(standard, unit = "pg/ml") {
  require(ggplot2)
  if (!is.data.frame(standard) && is.list(standard) && is.data.frame(standard$standard)) standard <- standard$standard
  data <- mutate(standard, file = paste("File", standard$file))
  x.scale <- filter(standard, type == "point")$x
  p <- ggplot(standard, aes(x = log10(x), y = y)) +
    geom_line(data = filter(standard, type == "curve"), size = 1) +
    geom_point(data = filter(standard, type == "point"), size = 4, shape = 20) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(paste("Standard concentration in", unit, "(log10 scale)"), breaks = log10(x.scale), labels = x.scale) +
    ylab("O.D. value") +
    facet_wrap(~ file)
  return(p)
}

#' Analyse the O.D. values (regression)
#'
#' Performs a 4-PL regression of the standard values and converts the O.D. into concentration values.
#'
#' @param .df dataframe containing at least the od and id columns (with O.D. values and sample identifiers).
#'
#' @param blank a logical value indicating whether blank values (id = 'blank') should be substracted from all O.D. values.
#'
#' @param log10 a logical value indicating whether O.D. values should be log10 transformed before the regression.
#'
#' @param std.key a character string specifying the common starting pattern of standard point ids (default = "STD").
#' 
#' @param dilution.column NULL or character string specifying a column to be used for the sample dilution factors (default = NULL).
#'
#' @param tecan a logical value indicating whether bad Tecan O.D. values (>1000) should be fixed.
#' 
#' @param multi.regression a logical value indicating whether the data set should be split by filename before the regression (when multiple files are loaded with 'elisa.load').
#'
#' @return A list object containing the standard curve and the modified input dataframe to include the calculated concentrations.
#'
#' @details A complete example on how to perform an analysis can be found at \url{http://eric.koncina.eu/r/elisar}.
#'
#' @examples
#' \dontrun{
#' library(elisar)
#' library(ggplot2)
#'
#' # Import file
#' e <- elisa.load("od_measure.xls")
#' e <- elisa.analyze(e)
#' e <- elisa.analyze(e, blank = TRUE, transform = TRUE)
#' }
#'
#' @export
elisa.analyse = function(.df, ..., multi.regression = TRUE) {
  if (!isTRUE(multi.regression)) return(elisa.analyse.single(.df, ...))
  
  .df <- .df %>%
    group_by(file) %>%
    do(result = elisa.analyse.single(., ...)) 
  
  .data <- .df %>%
    rowwise() %>%
    do(.$result$data) %>%
    ungroup()
  
  .standard <- .df %>%
    rowwise() %>%
    do(.$result$standard) %>%
    ungroup()
  
  return(list(standard = .standard, data = .data))
}

#' @rdname elisa.analyse
#' @export
elisa.analyze <- elisa.analyse

# elisa.analyse is able to handle a dataframe containing the data from multiple files.
# For each subset (file) it will call the elisa.analyse.single function unless multi.regression is set to FALSE
elisa.analyse.single = function(.df, blank = FALSE, transform = FALSE, tecan = FALSE, dilution.column = NULL, std.key = "STD") {
  if (!"id" %in% colnames(.df)) stop("Missing mandatory column 'id'")

  # Adjusting the dataframe (od can be log-transformed, blank substracted or fixed for a Tecan bug)
  .df <- .df %>%
    filter(tolower(id) != "empty") %>%
    mutate(y = od) %>% # y will be our "response" variable (od)
    mutate_if(is.true = tecan, y = ifelse(y > 1000, y/1000, y)) %>% # Tecan generates excel sheets with wrong values (locale bug?)
    mutate_if(is.true = blank, y = y - mean(y[tolower(id) == "blank"], na.rm = TRUE)) %>%
    mutate_if(is.true = transform, y = log10(y))

  # Creating a dataframe containing the standard curve points
  
  # We should only have a single file except if multi.regression is FALSE
  # In this case, for the standard curve, we substitute the distinct filenames by the collapsed names.
  .file <- paste(unique(.df$file), collapse = ", ")
  
  std <- .df %>%
    mutate(type = "point", file = .file) %>%
    filter(grepl(paste0("^", std.key), ignore.case = TRUE, id)) %>%
    mutate(id = gsub(",", ".", id), x = parse_number(id)) %>%
    mutate(log.x = log10(x)) %>%
    filter(!is.na(y), x > 0)

  if (nrow(std) < 4) stop("Not enough standard points to perform the regression...")

  # Performing the 4PL regression (with drc::drm)
  std.4PL <- drc::drm(y ~ log10(x), data = std, fct = drc::LL.4(), logDose = 10)

  # Extend the std dataframe and add points to draw the predicted curve
  std <- data.frame(log.x = seq(min(log10(std$x)), max(log10(std$x)), length.out = 100)) %>%
    mutate(x = 10^log.x, y = predict(std.4PL, .), type = "curve", file = .file) %>%
    bind_rows(std)  %>%
    select(file, type, x, y)
  
  # We use the inverse model (ED) to predict the concentration corresponding to the O.D values
  .df <- .df %>%
    bind_cols(tidy(suppressWarnings(drc::ED(std.4PL, .$y, type = "absolute", display = F)))) %>%
    rename(concentration = Estimate, concentration.sd = Std..Error) %>%
    mutate(concentration = ifelse(is.na(concentration) & y < summary(std.4PL)[[3]][[2]], 0, concentration)) %>%
    select(file, column, row, id, everything(), -.rownames, -y, -od, od)
  
    # Applying the dilution factor if present
  if (!is.null(dilution.column)) {
    if (!dilution.column %in% colnames(.df)) stop("Could not find specified dilution column!")
    .df <- .df %>%
      rename_(.dilution = dilution.column) %>%
      mutate(.dilution = ifelse(is.na(.dilution), 1, .dilution),
                concentration = .dilution * concentration,
                concentration.sd = .dilution * concentration.sd)
  }
  return(list(standard = std, data = .df))
}
