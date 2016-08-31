#' @import digest
#' @import readr
#' @import readxl
#' @import dplyr
#' @importFrom purrr map_lgl map
#' @importFrom tidyr gather
#' @import broom

mutate_ifTrue <- function(.data, is.true = TRUE, ...) {
  if (isTRUE(is.true)) return(mutate(.data, ...))
  else return(.data)
}

# Adding a "glance" method for drc (see broom)
#' @export
glance.drc <- function(x, ...) {
  .row <- NULL
  .row <- rbind(.row, x$coefficients)
  return(as.data.frame(.row))
}

# Calculate the concentrations from the 4PL model using drc::ED
od2conc <- function(.df, .model) {
  # From: http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings
  h <- function(w) if( any( grepl( "log\\(\\(100 - p\\)/100\\).*NaN.*", w) ) ) invokeRestart( "muffleWarning" )
  .df %>%
    bind_cols(rename(tidy(withCallingHandlers(drc::ED(.model, .df[[".y"]], type = "absolute", display = F), warning = h)), .c = Estimate, .c.sd = Std..Error)) %>%
    mutate(.c = ifelse(is.na(.c) & .y < summary(.model)[[3]][[2]], 0, .c)) -> .df
  return(.df)
}

#' @export
print.elisa_df <- function(x, ...) {
  .model <- attr(x, "model")
  cat("elisa.analyse() concentration values obtained from the OD with the following 4PL regression(s):\n\n")
  print(as.data.frame(.model))
  cat("\n")
  print(tbl_df(unclass(x)))
}

#' Extract the standard points
#'
#' Filter out the standard point values according the standard point identification pattern (default = "STD").
#' 
#' @param .df dataframe containing at least the od and id columns (with O.D. values and sample identifiers).
#' 
#' @param std.key a character string specifying the common starting pattern of standard point ids (default = "STD").
#' 
#' @param od a character string specifying the column containing the od values (default = "value").
#' 
#' @param .keep a vector containing the column names which should be included in the output
#' 
#' @return A dataframe containing the standard.
#'
#' @export
elisa.standard <- function(.df, std.key = "STD", od = "value", .keep = NULL) {
  if (!is.null(.keep) && !.keep %in% colnames(.df)) stop("Cannot keep a column that does not exist")
  .keep <- c("column", "row", "id", "x", od, .keep)
  if ("file" %in% colnames(.df)) .keep <- c("file", .keep)
  std <- .df %>%
    filter(grepl(paste0("^", std.key), ignore.case = TRUE, id)) %>%
    mutate(id = gsub(",", ".", id), x = parse_number(id)) %>%
    filter(x > 0) %>%
    select(one_of(.keep))
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
#' @param concentration a character string specifying the column that will contain the calculated concentration values (default = "concentration").
#'
#' @param blank a logical value indicating whether blank values (id = 'blank') should be substracted from all O.D. values.
#'
#' @param transform a logical value indicating whether O.D. values should be log10 transformed before the regression.
#'
#' @param std.key a character string specifying the common starting pattern of standard point ids (default = "STD").
#' 
#' @param dilution NULL or character string specifying a column to be used for the sample dilution factors (default = NULL).
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
elisa.analyse = function(.df, blank = FALSE, transform = FALSE, tecan = FALSE, dilution = NULL, std.key = "STD", od = "value", concentration = "concentration", multi.regression = TRUE) {
  
  if (!"id" %in% colnames(.df)) stop("Missing mandatory column 'id'")
  if (concentration %in% colnames(.df)) stop("Concentration column already exists (try to adjust the 'concentration' argument).")
  if (any(c(".y", ".c", ".c.sd", ".dilution") %in% colnames(.df))) stop("The dataframe should not contain column names .y, .dilution, .c or .c.sd")
  
  if (!isTRUE(multi.regression)) {
    .df %>%
      mutate(file = paste(unique(file), collapse = ", ")) -> .df
  }
  
  # Adjusting the dataframe (od can be log-transformed, blank substracted or fixed for a Tecan bug)
  # and performing the 4PL regression
  .df %>%
    filter(tolower(id) != "empty") %>%
    mutate_(.y = od) %>% # y will be our "response" variable (od)
    group_by(file) %>%
    mutate_ifTrue(is.true = tecan, .y = ifelse(.y > 1000, .y/1000, .y)) %>% # Tecan generates excel sheets with wrong values (locale bug?)
    mutate_ifTrue(is.true = blank, .y = .y - mean(.y[tolower(id) == "blank"], na.rm = TRUE)) %>%
    mutate_ifTrue(is.true = transform, .y = log10(.y)) %>%
    nest() %>%
    mutate(std = map(data, elisa.standard, .keep = ".y")) %>%
    mutate(model = map(std, ~ drc::drm(.y ~ log10(x), data = .x, fct = drc::LL.4(names = c("Slope", "Lower", "Upper", "ED50")), logDose = 10))) %>%
    mutate(data = map(data, od2conc, .m = model[[1]])) -> .df
  
  .df %>%
    select(-data, -std) %>%
    mutate(model = map(model, glance)) %>%
    unnest() -> .model
  
  .df %>%
    select(-std, -model) %>%
    unnest() %>%
    group_by(file) %>%
    mutate(.valid = ifelse(.y <= max(.y[grepl(paste0("^", std.key), ignore.case = TRUE, id)]) & !is.na(.y), TRUE, FALSE)) %>%
    ungroup -> .df
  
  # Applying the dilution factor if present
  if (!is.null(dilution)) {
    if (!dilution %in% colnames(.df)) stop("Could not find specified dilution column!")
    .df <- .df %>%
      mutate_(.dilution = dilution) %>%
      mutate(.dilution = ifelse(is.na(.dilution), 1, .dilution),
             .c = .dilution * .c,
             .c.sd = .dilution * .c.sd) %>%
      select(-.dilution)
  }
  
  .df <- .df %>%
    select(file, column, row, id, everything(), -.rownames, -.y, -.valid, .valid) %>%
    rename_(.dots = setNames(c(".c", ".c.sd"), c(concentration, paste0(concentration, ".sd"))))
  
  # Displaying warning if OD is outside standard range
  if (!.df %>% filter(!grepl(paste0("^", std.key), ignore.case = TRUE, id)) %>% select(.valid) %>% map_lgl(all)) 
    message(sprintf("%d OD value%s outside the standard range",
                    s <- sum(!.df$.valid, na.rm = TRUE), ifelse(s > 1, "s are", " is")))
  
  # Displaying warning if a standard point is missing (NA as a consequence of a tecan "Overflow")
  if (!.df %>% filter(grepl(paste0("^", std.key), ignore.case = TRUE, id)) %>% select(.valid) %>% map_lgl(all))
    message(sprintf("%d standard OD value%s invalid (NA or overflow)",
                    s <- sum(!.df$.valid, na.rm = TRUE), ifelse(s > 1, "s are", " is")))
  
  if (isTRUE(transform)) attr(.df, "transform") <- TRUE
  attr(.df, "model") <- .model
  class(.df) <- append("elisa_df", class(.df))
  
  return(.df)
}

#' @rdname elisa.analyse
#' @export
elisa.analyze <- elisa.analyse
