#' @import dplyr
#' @import tidyr
#' @import readxl
#' @import digest

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

find.plate <- function(.i, .input) {
  .df <- tryCatch({ read_excel(.input, col_names = FALSE, sheet = .i)}
                  , error = function(e) {
                    message(e)
                    return(NULL)
                  }
  )
  if (is.null(.df)) return(NULL)
  maxrow <- nrow(.df) - 8
  maxcol <- ncol(.df) - 12
  for(col in 1:maxcol) {
    for(row in 1:maxrow) {
      if (isTRUE(all(.df[row, 1:12 + col] == 1:12)) & isTRUE(all(.df[1:8 + row, col] == LETTERS[1:8]))) {
        .df <- tryCatch({
          setNames(.df[1:8 + row, 0:12 + col], c("row", 1:12)) %>%
            mutate_each(funs(as.numeric), -row) %>%
            gather_(key = "column", value = "value", c(1:12)) %>%
            `attr<-`("what", "data") %>%
            `attr<-`("pos", c(row, col))
        }, warning = function(w) {
          # I would like to specifically catch the NA warnings...
          # I can get the message (w) but it depends on locales...
          # Is it possible to obtain an id/code or whatever unique?
          .id <- tryCatch({setNames(.df[1:8 + row, 0:12 + col], c("row", 1:12)) %>%
              gather_(key = "column", value = "id", c(1:12))
          }, warning = function(w) {
            # If we still raise a warning, then something is wrong!
            stop(paste("Could not handle the sheet", .i))
          })
          .id <- tbl_df(as.data.frame(sub("\\.0{6}$", "", as.matrix(.id)), stringsAsFactors = FALSE))
          # Trying to read extended ID table
          .id.ext <- tryCatch({ read_excel(.input, sheet = .i, skip = row + 8)}
                              , error = function(e) {
                                message("Could not find extended ID table... Ignoring")
                                return(NULL)
                              }
          )
          if (is.data.frame(.id.ext)) {
            if (! "id" %in% names(.id.ext)) {
              message("Could not join ID table: missing id column")
            } else {
              .id <- full_join(.id, .id.ext, by = c("id"))
            }
          }
          attr(.id, "what") <- "id"
          attr(.id, "pos") <- c(row, col)
          return(.id)
        })
        return(.df)
      }
    }
  }
  return(NULL)
}

read.plate.single <- function(input, layout = NA, checksum = "md5") {
  # Checking the extension
  ext <- gsub(".*\\.([[:alnum:]]+)$", "\\1", input)
  if (!ext %in% c("xls", "xlsx")) stop("Wrong input file format! (expecting an xls or xlsx Excel file)")
  
  # Checking the checksum argument:
  algo <- c("md5", "sha1", "crc32", "sha256", "sha512")
  if (!checksum  %in% c(algo)) stop("Unknown checksum algorithm...")
  
  sheet.names <- excel_sheets(input)
  l <- lapply(sheet.names, find.plate, input)
  content <- lapply(l, attr, "what") %>% unlist()
  
  .df <- l[which(grepl("^data$", content))] %>%
    bind_rows(.id = "sheet") %>%
    # I hope that I'm not introducing a bug here...
    mutate(sheet = sheet.names[which(grepl("^data$", content))][as.numeric(sheet)])
  
  data.checksum <- digest(.df, algo = checksum)
  
  .id <- which(grepl("^id$", content))
  if (!is.na(layout)) {
    
    if (is.character(layout) && file.exists(layout)) {
      message(sprintf("Using external layout file %s in %s", basename(layout), basename(input)))
      # Setting or overriding by layout file (must contain a single ID sheet)
      if (length(.id) != 0) {
        # Layout was already detected but layout argument overrides autodetection
        message(sprintf("Overriding detected layout in %s", input))
        l[.id] <- NULL
      }
      layout.sheetnames <- excel_sheets(layout)
      layout.l <- lapply(layout.sheetnames, find.plate, layout)
      layout.content <- lapply(layout.l, attr, "what") %>% unlist()
      l <- append(l, layout.l[which(grepl("^id$", layout.content))]) # Removing all elements which are not detected as possible IDs
      # Computing content and .id again for further processing
      content <- lapply(l, attr, "what") %>% unlist()
      .id <- which(grepl("^id$", content))
    } else if (layout %in% sheet.names) {
      # Overriding by sheet name
      message(sprintf("Forcing layout to sheet %s in %s", layout, basename(input)))
      .id <- which(grepl(paste0("^", layout, "$"), sheet.names))
    } else if (is.numeric(layout) && layout %in% 1:length(sheet.names)) {
      # Overriding by sheet index
      message(sprintf("Forcing layout to sheet %s in %s", layout, basename(input)))
      .id <- layout
    } else {
      # The layout argument cannot be used...
      stop("Bad layout argument")
    }
  }
  # We should obtain a single "id" datasheet:
  
  if (length(.id) > 1) {
    message("I detected more than a single layout sheet... skipping")
  } else if (length(.id) == 0) {
    message("I detected no layout sheet... skipping")
  } else {
    if (nrow(.df) > 0) {
      if (.id %in% which(grepl("^data$", content))) warning("Layout sheet was detected as a sheet with measures... This will lead to unexpected results!")
      .df <- full_join(l[[.id]], .df, by = c("row", "column"))
    } else {
      message("I detected no sheet with measures... skipping")
      .df <- l[[.id]]
    }
  }
  
  # this will remove ids set to "empty" or NA
  if ("id" %in% names(.df)) .df <- .df %>% filter(tolower(id) != "empty")
  
  attr(.df, checksum) <- data.checksum
  if (ext == "xls" && is.readxl.bugging(.df)) message("This xls file might have generated incorrect text values. Consider converting xls to xlsx!")
  return(.df)
}

read.plate.single.caller <- function(i, input, layout = NA, checksum = "md5") {
  read.plate.single(input = input[i], layout = layout[i], checksum = checksum)
}

#' Import the OD/luminescence/fluorescence measures from plate reader exported excel sheets (like Fluostar Optima, or Tecan Sunrise)
#'
#' Reads excel files exported from the MARS software (\url{http://www.bmglabtech.com/en/products/software/mars-data-analysis/}) or Tecan Magellan (\url{http://lifesciences.tecan.com/products/software/magellan_data_analysis_software}) and modified to include the plate layout (see documentation).
#'
#' @param input vector containing the path(s) to the input file(s)
#' 
#' @param layout If NA, layout sheets are auto-detected in each input file. Setting layout with paths to layout files overrides the autodetection. The vector must be of same size as input or equal to 1 (overrides all input elements with the same layout).
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
#' df <- read.plate("od_measure.xls")
#' df <- read.plate(c("od_measure1.xls", "od_measure2.xls"))
#' df <- read.plate(c("od_measure1.xls", "od_measure2.xls"), layout = c(NA, "layout2.xls"))
#' }
#'
#' @export
read.plate = function(input, layout = NA, checksum = "md5") {
  input <- unique(normalizePath(input)) # Removing potential duplicated file references
  names(input) <- basename(input)
  # Using method from the following code to pass multiple arguments with lapply (input and layout pairs)
  # http://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
  
  if (length(input) != length(layout) && length(layout) > 1) {
    stop("layout argument should have a length of 1 or equal to input!")
  }
  if (length(input) > 1) layout <-  rep(layout, length(input))
  l <- lapply(seq_along(input), read.plate.single.caller, input = input, layout = layout, checksum = checksum)
  names(l) <- basename(input)
  cs <- unlist(lapply(l, function(x){attributes(x)[checksum][[1]]}))
  .df <- l %>% bind_rows(.id = "file")
  attr(.df, checksum) <- cs
  return(.df)
}
