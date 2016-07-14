#' @import digest
#' @import readr
#' @import readxl
#' @import dplyr
#' @importFrom tidyr gather
#' @import broom

mutate_if <- function(.data, is.true = TRUE, ...) {
  if (isTRUE(is.true)) return(mutate(.data, ...))
  else return(.data)
}

# Adding a "glance" method for drc (see broom)
glance.drc <- function(x, ...) {
  .row <- NULL
  .row <- rbind(.row, x$coefficients)
  return(as.data.frame(.row))
}

#' @export
print.elisa_df <- function(x, ...) {
  .model <- attr(x, "model")
  cat("elisa.analyse() concentration values obtained from the OD with the following 4PL regression(s):\n\n")
  print(as.data.frame(.model))
  cat("\n")
  print(tbl_df(x))
}

#' Extract the standard points
#'
#' Filter out the standard point values according the standard point identification pattern (default = "STD").
#' 
#' @param .df dataframe containing at least the od and id columns (with O.D. values and sample identifiers).
#' 
#' @param std.key a character string specifying the common starting pattern of standard point ids (default = "STD").
#' 
#' @return A dataframe containing the standard.
#'
#' @export
elisa.standard <- function(.df, std.key = "STD") {
  std <- .df %>%
    group_by(file) %>%
    filter(grepl(paste0("^", std.key), ignore.case = TRUE, id)) %>%
    mutate(id = gsub(",", ".", id), x = parse_number(id)) %>%
    filter(!is.na(od), x > 0) %>%
    select(file, column, row, id, x, od, concentration)
  return(std)
}


#' Analyse the O.D. values (regression)
#'
#' Performs a 4-PL regression of the standard values and converts the O.D. into concentration values.
#'
#' @param .df dataframe containing at least the od and id columns (with O.D. values and sample identifiers).
#'
#' @param od a character string specifying the column containing the od values (default = "value").
#'
#' @param blank a logical value indicating whether blank values (id = 'blank') should be substracted from all O.D. values.
#'
#' @param transform a logical value indicating whether O.D. values should be log10 transformed before the regression.
#'
#' @param std.key a character string specifying the common starting pattern of standard point ids (default = "STD").
#' 
#' @param dilution.column NULL or character string specifying a column to be used for the sample dilution factors (default = NULL).
#'
#' @param tecan a logical value indicating whether bad Tecan O.D. values (>1000) should be fixed.
#' 
#' @param multi.regression a logical value indicating whether the data set should be split by filename before the regression (when multiple files are loaded with 'read.plate').
#'
#' @return A dataframe including the calculated concentrations.
#'
#' @details A complete example on how to perform an analysis can be found at \url{http://eric.koncina.eu/r/elisar}.
#'
#' @examples
#' \dontrun{
#' library(elisar)
#' library(ggplot2)
#'
#' # Import file
#' e <- read.plate("od_measure.xls")
#' e <- elisa.analyze(e)
#' e <- elisa.analyze(e, blank = TRUE, transform = TRUE)
#' }
#'
#' @export
elisa.analyse = function(.df, blank = FALSE, transform = FALSE, tecan = FALSE, dilution.column = NULL, std.key = "STD", od = "value", multi.regression = TRUE) {
  if (!isTRUE(multi.regression)) {
    .df <- .df %>%
      elisa.analyse.single(., blank = blank, transform = transform, tecan = tecan, dilution.column = dilution.column, std.key = std.key, od = od)
    .data <- .df$data
    .model <- .df$model %>%
      glance()
  } else {
    .df <- .df %>%
      group_by(file) %>%
      do(result = elisa.analyse.single(., blank = blank, transform = transform, tecan = tecan, dilution.column = dilution.column, std.key = std.key, od = od))
    
    .data <- .df %>%
      rowwise() %>%
      do(.$result$data) %>%
      ungroup()
    
    .model <- .df %>%
      ungroup() %>%
      rowwise() %>%
      do(bind_cols(data.frame(file = .$file, stringsAsFactors = FALSE), glance(.$result$model)))
  }
  
  if (isTRUE(transform)) attr(.data, "transform") <- TRUE
  attr(.data, "model") <- .model
  class(.data) <- unique(c("elisa_df", class(.data)))
  
  return(.data)
}

#' @rdname elisa.analyse
#' @export
elisa.analyze <- elisa.analyse

# elisa.analyse is able to handle a dataframe containing the data from multiple files.
# For each subset (file) it will call the elisa.analyse.single function unless multi.regression is set to FALSE
elisa.analyse.single = function(.df, blank = FALSE, transform = FALSE, tecan = FALSE, dilution.column = NULL, std.key = "STD", od = "value") {
  if (!"id" %in% colnames(.df)) stop("Missing mandatory column 'id'")
  
  # Adjusting the dataframe (od can be log-transformed, blank substracted or fixed for a Tecan bug)
  .df <- .df %>%
    filter(tolower(id) != "empty") %>%
    rename_(od = od) %>%
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
  std.4PL <- drc::drm(y ~ log10(x), data = std, fct = drc::LL.4(names = c("Slope", "Lower", "Upper", "ED50")), logDose = 10)
  
  # We use the inverse model (ED) to predict the concentration corresponding to the O.D values
  .df <- .df %>%
    bind_cols(tidy(suppressWarnings(drc::ED(std.4PL, .$y, type = "absolute", display = F)))) %>%
    rename(concentration = Estimate, concentration.sd = Std..Error) %>%
    mutate(concentration = ifelse(is.na(concentration) & y < summary(std.4PL)[[3]][[2]], 0, concentration)) %>%
    mutate(.valid = ifelse(od <= max(std$od), TRUE, FALSE)) %>%
    select(file, column, row, id, everything(), -.rownames, -y, -od, od, -.valid, .valid)
  
  # Applying the dilution factor if present
  if (!is.null(dilution.column)) {
    if (!dilution.column %in% colnames(.df)) stop("Could not find specified dilution column!")
    .df <- .df %>%
      mutate_(.dilution = dilution.column) %>%
      mutate(.dilution = ifelse(is.na(.dilution), 1, .dilution),
             concentration = .dilution * concentration,
             concentration.sd = .dilution * concentration.sd) %>%
      select(-.dilution)
  }
  
  # Displaying warning if OD is outside standard range
  if (!all(.df$.valid)) message(sprintf("%d OD value%s outside the standard range",
                                        s <- sum(!.df$.valid, na.rm = TRUE), ifelse(s > 1, "s are", " is")))
  return(list(model = std.4PL, data = .df))
}
