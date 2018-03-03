#' @import readxl
#' @importFrom drc LL.4 drm
#' @importFrom utils installed.packages type.convert

NULL

std_fit <- function(concentration, od) {
  warn_list <- NULL
  fit_env <- environment()
  h <- function(w) {assign("warn_list", c(warn_list, w[["message"]]), envir = fit_env); invokeRestart("muffleWarning")}
  model <- withCallingHandlers(drc::drm(od ~ log10(concentration), fct = drc::LL.4(names = c("Slope", "Lower", "Upper", "ED50")), logDose = 10), warning = h)
  lapply(unique(warn_list), function(x) warning(paste(x, "(during standard curve fitting)\n"), call. = FALSE))
  model
}

drm_estimate <- function(drm_model, od) {
  # From: http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings
  h <- function(w) if(any(grepl("log\\(\\(100 - p\\)/100\\).*NaN.*", w))) invokeRestart( "muffleWarning" )
  conc <- withCallingHandlers(drc::ED(drm_model, od, type = "absolute", display = FALSE), warning = h)
  
  # Replace NaN values with 0 when OD < lower estimate
  conc[,1] <- replace(conc[,1], is.nan(conc[,1]) & od < drm_model[["coefficients"]][[2]], 0)
  # Replace NaN values of sd with NA if concentration was changed above
  conc[,2] <- replace(conc[,2], is.nan(conc[,2]) & conc[,1] == 0, NA)
  rownames(conc) <- NULL
  in_range <- ifelse(od <= max(drm_model[["data"]]["od"]), TRUE, FALSE)
  if ((s <- sum(!in_range, na.rm = TRUE)) > 0) warning(sprintf("%d OD value%s outside the standard range",
                                                               s, ifelse(s > 1, "s are", " is")), call. = FALSE)
  list(estimate = conc, in_range = in_range)
}

# Parse standard concentration values and return the values together
# with the index of standard points
get_standard <- function(id, std_key = "^STD", dec = ".") {
  std_index <- grepl(std_key, id)
  if (sum(std_index) == 0) stop("Could not detect any matching standard curve ID")
  std_value <- type.convert(sub(std_key, "", id[std_index]), as.is = TRUE, dec = dec)
  if (!is.numeric(std_value)) stop("Failed to parse standard concentration values")
  list(index = std_index, value = std_value)
}


#' Extract the standard curve
#'
#' Parse the id for the standard curve key and extract the concentration and associated O.D. values.
#' 
#' @param .data dataframe containing at least the value and id columns (with O.D. values and sample identifiers).
#' 
#' @param x name of the concentration column
#' 
#' @param y name of the O.D. column
#' 
#' @param std_key a character string specifying the common starting pattern of standard point ids (default = "^STD").
#' 
#' @param dec a character string used as a decimal separator for the encoded standard concentration values.
#' 
#' @return A data frame containing the standard curve
#'
#' @export
extract_standard <- function(.data, x = "x", y = "y", std_key = "^STD", dec = ".") {
  x <- get_arg(substitute(x))
  y <- get_arg(substitute(y))
  std <- get_standard(.data[["id"]], std_key, dec)
  std <- data.frame(
    x = std[["value"]],
    y = .data[["value"]][std[["index"]]]
  )
  
  colnames(std) <- c(x, y)
  std
}

#' Analyse the O.D. values (regression)
#'
#' Performs a 4-PL regression of the standard values and converts the O.D. into concentration values.
#'
#' @param id a character vector containing the identifiers.
#'
#' @param value a numerical vector containing the O.D. values.
#'
#' @param std_key a character string specifying the common starting pattern of standard point ids (default = "^STD").
#' 
#' @param dec a character string used as a decimal separator for the encoded standard concentration values.
#' 
#' @return A numerical vector containing the calculated concentrations.
#'
#' @details A complete example on how to perform an analysis can be found at \url{https://github.com/koncina/elisar}.
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(elisar)
#' 
#' read_plate("od_measure.xls") %>%
#'   mutate(concentration = get_concentration(id, value))
#' }
#'
#' @export
od_to_concentration <- function(id, value, std_key = "^STD", dec = ".") {
  std <- get_standard(id, std_key, dec)
  drm_model <- std_fit(std[["value"]], value[std[["index"]]])
  conc <- drm_estimate(drm_model, value)
  as.numeric(conc[["estimate"]][,1])
}

#' Analyse the O.D. values (regression)
#'
#' Performs a 4-PL regression of the standard values and converts the O.D. into concentration values.
#'
#' @param .data dataframe containing at least the value and id columns (with O.D. values and sample identifiers).
#'
#' @param value a character string specifying the column containing the O.D. values (default = "value").
#'
#' @param std_key a character string specifying the common starting pattern of standard point ids (default = "^STD").
#' 
#' @param dec a character string used as a decimal separator for the encoded standard concentration values.
#' 
#' @param var_in a character string used for the OD values (default = "value")
#' 
#' @param var_out a character string used to name the output columns (default = "estimate")
#' 
#' @param .drop Should input columns be dropped? (default = `FALSE`)
#' 
#' @return A dataframe including the calculated concentrations, standard deviation and wether the value is in the range of the standard curve.
#'
#' @details A complete example on how to perform an analysis can be found at \url{https://github.com/koncina/elisar}.
#'
#' @examples
#' \dontrun{
#' library(elisar)
#' # Import file
#' e <- read_plate("od_measure.xls")
#' elisa_analyse(e)
#' elisa_analyse(e, .drop = TRUE)
#' }
#'
#' @export
elisa_analyse <- function(.data, std_key = "^STD", dec = ".", var_in = "value", var_out = "estimate", .drop = FALSE) {
  check_arg(var_in, var_out, var_type = "character", var_length = 1)
  std <- get_standard(.data[["id"]], std_key, dec)
  drm_model <- std_fit(std[[var_in]], .data[[var_in]][std[["index"]]])
  conc <- drm_estimate(drm_model,  .data[[var_in]])
  conc <- as.data.frame(conc)
  colnames(conc) <- c(var_out, paste0(var_out, "_std_err"), "in_range")
  if (!isTRUE(.drop)) conc <- cbind(.data, conc)
  class(conc) <- c("tbl_df", "tbl", "data.frame")
  conc
}

#' @rdname elisa_analyse
#' @export
elisa_analyze <- elisa_analyse