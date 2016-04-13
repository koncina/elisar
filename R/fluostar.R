#' @import dplyr
#' @import tidyr
#' @import readxl

getFSDataframe <- function(.i, .input) {
  .df <- tryCatch({ read_excel(.input, col_names = FALSE, sheet = .i)}
                  , error = function(e) {
                    message(e)
                    return(NULL)
                  }
  )
  if (is.null(.df)) return(NULL)
  if (all(.df[6, 2:13] == 1:12) & all(.df[7:14,1] == LETTERS[1:8])) {
    # We detected the plate
    # Let us extract the raw data:
    .df <- tryCatch({
      setNames(.df[7:14, 1:13], c("row", 1:12)) %>%
        mutate_each(funs(as.numeric), -row) %>%
        gather_(key = "column", value = tolower(.df[4, 2]), c(1:12)) %>%
        `attr<-`("what", "data")
    }, warning = function(w) {
      # I would like to specifically catch the NA warnings...
      # I can get the message (w) but it depends on locales...
      # Is it possible to obtain an id/code or whatever unique?
      id <- tryCatch({setNames(.df[7:14, 1:13], c("row", 1:12)) %>%
          gather_(key = "column", value = "id", c(1:12))
      }, warning = function(w) {
        # If we still raise a warning, then something is wrong!
        stop(paste("Could not handle the sheet", .i))
      })
      id <- tbl_df(as.data.frame(sub("\\.0{6}$", "", as.matrix(id)), stringsAsFactors = FALSE))
      attr(id, "what") <- "id"
      return(id)
    })
  } else {
    .df <- NULL
    message(paste("Could not detect values on sheet", .i, ": skipping!"))
  }
  return(.df)
}

read.fluostar.single <- function(input, checksum = "md5", verbose = FALSE) {
  # Checking the extension
  ext <- gsub(".*\\.([[:alnum:]]+)$", "\\1", input)
  if (!ext %in% c("xls", "xlsx")) stop("Wrong input file format! (expecting an xls or xlsx Excel file)")
  
  # Checking the checksum argument:
  algo <- c("md5", "sha1", "crc32", "sha256", "sha512")
  if (!checksum  %in% c(algo)) stop("Unknown checksum algorithm...")
  
  sheet.names <- excel_sheets(input)
  l <- lapply(sheet.names, getFSDataframe, input)
  content <- lapply(l, attr, "what") %>% unlist()
  .df <- l[which(grepl("^data$", content))] %>%
    bind_rows(.id = "sheet") %>%
    # I hope that I'm not introducing a bug here...
    mutate(sheet = sheet.names[which(grepl("^data$", content))][as.numeric(sheet)])
  
  if (length(names(.df)) != 4) stop("Unexpected number of columns: check your input file")
  
  data.checksum <- digest(.df, algo = checksum)
  
  # We should obtain a single "id" datasheet:
  .id <- which(grepl("^id$", content))
  if (length(.id) > 1) {
    message("I detected more than a single layout sheet... skipping")
  } else if (length(.id) == 0) {
    message("I detected no layout sheet... skipping")
  } else {
    .df <- full_join(l[[.id]], .df, by = c("row", "column"))
    
    .id <- tryCatch({ read_excel(input, sheet = .id, skip = 14)}
                    , error = function(e) {
                      if (isTRUE(verbose)) message(e)
                      return(NULL)
                    }
    )
    if (is.data.frame(.id)) {
      if (! "id" %in% names(.id)) {
        message("Missing id column")
      } else {
        # Disabling: If required, reenable it and apply it only to string columns!
        #.id <- tbl_df(as.data.frame(sub("\\.0{6}$", "", as.matrix(.id)), stringsAsFactors = FALSE))
        .df <- full_join(.df, .id, by = c("id"))
      }
    }
    
  }
  
  # this will remove ids set to "empty" or NA
  .df <- .df %>% filter(tolower(id) != "empty")
  
  attr(.df, checksum) <- data.checksum
  if (ext == "xls" && is.readxl.bugging(.df)) message("Detected suspicious text values during import. Consider converting xls to xlsx!")
  
  return(.df)
}

#' Import the OD/luminescence/fluorescence measures from the Fluostar Optima excel sheets
#'
#' Reads excel files exported from the MARS software (\url{http://www.bmglabtech.com/en/products/software/mars-data-analysis/}) and modified to include the plate layout (see documentation).
#'
#' @param input vector containing the path(s) to the input file(s)
#' 
#' @param checksum a character string specifying the algorithm to calculate the data checksum (defaults to "md5"). checksum can be one of c("md5", "sha1", "crc32", "sha256", "sha512").
#'                  The checksum is stored in the attributes.
#'
#' @details Example on how to prepare the excel file can be found at \url{http://eric.koncina.eu/r/elisar}.
#'
#' @examples
#' \dontrun{
#' library(elisar)
#'
#' # Import file(s)
#' df <- read.fluostar("od_measure.xls")
#' df <- read.fluostar(c("od_measure1.xls", "od_measure2.xls"))
#' }
#'
#' @export
read.fluostar = function(input, checksum = "md5", verbose = FALSE) {
  input <- unique(normalizePath(input)) # Removing potential duplicated file references
  names(input) <- basename(input)
  l <- lapply(input, read.fluostar.single, checksum = checksum, verbose = verbose)
  cs <- unlist(lapply(l, function(x){attributes(x)[checksum][[1]]}))
  .df <- l %>% bind_rows(.id = "file")
  attr(.df, checksum) <- cs
  return(.df)
}
