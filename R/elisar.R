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

#' Draws the standard curve of the elisa analysis
#'
#' Requires ggplot2
#'
#' @param standard list object (or dataframe) returned by the elisa.analyse function
#'
#' @param concentration unit of the standard points (defaults to NULL)
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
#' elisa.standard(e)
#' # OR supplying the dataframe
#' elisa.standard(e$standard)
#' }
#'
#' @export
elisa.standard = function(standard, unit = NULL) {
  require(ggplot2)
  if (!is.data.frame(standard) && is.list(standard) && is.data.frame(standard$standard)) standard <- standard$standard
  data <- mutate(standard, file = paste("File", standard$file))
  x.scale <- filter(standard, type == "point")$x
  p <- ggplot(standard, aes(x = log10(x), y = y)) +
    geom_line(data = filter(standard, type == "curve"), size = 1) +
    geom_point(data = filter(standard, type == "point"), size = 4, shape = 20) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(sprintf("Standard concentration%s (log10 scale)", ifelse(!is.null(unit), paste(" in", unit), "")), breaks = log10(x.scale), labels = x.scale) +
    ylab(ifelse(isTRUE(attr(standard, "log")), "log10(OD)", "OD")) +
    facet_wrap(~ file)
  return(p)
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
#' e <- read.plate("od_measure.xls")
#' e <- elisa.analyze(e)
#' e <- elisa.analyze(e, blank = TRUE, transform = TRUE)
#' }
#'
#' @export
elisa.analyse = function(.df, ..., transform = FALSE, multi.regression = TRUE) {
  if (!isTRUE(multi.regression)) return(elisa.analyse.single(.df, transform = transform, ...))
  
  .df <- .df %>%
    group_by(file) %>%
    do(result = elisa.analyse.single(., transform = transform, ...)) 
  
  .data <- .df %>%
    rowwise() %>%
    do(.$result$data) %>%
    ungroup()
  
  .standard <- .df %>%
    rowwise() %>%
    do(.$result$standard) %>%
    ungroup()
  
  if (isTRUE(transform)) attr(.standard, "log") <- TRUE
  
  return(list(standard = .standard, data = .data))
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
  std.4PL <- drc::drm(y ~ log10(x), data = std, fct = drc::LL.4(), logDose = 10)
  
  # We use the inverse model (ED) to predict the concentration corresponding to the O.D values
  .df <- .df %>%
    bind_cols(tidy(suppressWarnings(drc::ED(std.4PL, .$y, type = "absolute", display = F)))) %>%
    rename(concentration = Estimate, concentration.sd = Std..Error) %>%
    mutate(concentration = ifelse(is.na(concentration) & y < summary(std.4PL)[[3]][[2]], 0, concentration)) %>%
    mutate(.valid = ifelse(od <= max(std$od), TRUE, FALSE)) %>%
    select(file, column, row, id, everything(), -.rownames, -y, -od, od, -.valid, .valid)
  
  # Extend the std dataframe and add points to draw the predicted curve
  std <- data.frame(log.x = seq(min(log10(std$x)), max(log10(std$x)), length.out = 100)) %>%
    mutate(x = 10^log.x, y = predict(std.4PL, .), type = "curve", file = .file) %>%
    bind_rows(std) %>%
    select(file, type, x, y)
  
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
  
  return(list(standard = std, data = .df))
}
