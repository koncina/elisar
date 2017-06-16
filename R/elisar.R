#' @import digest
#' @import readr
#' @import readxl
#' @import dplyr
#' @importFrom purrr map_lgl map safely quietly
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
#' @param .key a character string specifying the common starting pattern of standard point ids (default = "STD").
#' 
#' @param .od the column containing the od values (default = value).
#' 
#' @param .dose the column that will contain the extracted concentration values
#' 
#' @param .drop A boolean specifying whether additional columns should be dropped (default) or not.
#' 
#' @return A dataframe containing the standard with the extracted concentration value (conc).
#'
#' @export
get_standard <- function(.df, .key = "STD", .od = value, .dose = .dose, .drop = TRUE) {
  .od <- enquo(.od)
  # How can I assign .dose = .dose? the quosure is empty...
  .dose <- enquo(.dose)
  if (rlang::quo_is_missing(.dose)) .dose <- quo(.dose)
  .df %>%
    filter(grepl(paste0("^", .key), ignore.case = TRUE, id)) %>%
    mutate(id = gsub(",", ".", id),
           !!quo_name(.dose) := parse_number(id)) %>%
    select(!!!c(quos(file, col, row, id), if (!.drop) quos(everything()), quos(-!!.dose, -!!.od, !!.dose, !!.od)))
}

# Define a tidyverse compliant data first and safe drc::drm function. Using curveid would require more adjustement
# We define otrace = TRUE to remove messages from optim (probable bug in drm as messages should be disabled with otrace = FALSE)
safe_drm_lite <- purrr::safely(function(data, ...) drc::drm(data = data, control = drc::drmc(otrace = TRUE), ...))

# Define a tidyverse compliant data first drc::ED function to estimate effective doses (we will return NA if no model is provided)
estimate <- function(data, drm, ...) {
  if (is_null(drm)) return(tibble(estimate = rep(NA_real_, length(data)), std_error = estimate))
  drc::ED(drm, data, ...) %>%
    as_tibble() %>%
    set_names(c("estimate", "std_error"))
}

#' @export
elisa_analyse <- function(.df, .od = value, .ignore = c("empty"), .dilution = NULL) {
  .od <- enquo(.od)
  .dilution <- enquo(.dilution)
  
  #check_quosures <- c(.od, .dilution)
  #if (!all(check_quosures %>% map_lgl(rlang::quo_is_symbolic) | check_quosures %>% map_lgl(rlang::quo_is_null))) stop("")
  
  mandatory_columns <- c(quo_name(.od), "id")
  if (!is.null(rlang::get_expr(.dilution))) {
    mandatory_columns <- c(mandatory_columns, quo_name(.dilution))
  }
  
  check_columns <- mandatory_columns %in% colnames(.df)
  if (!all(check_columns)) stop(glue::glue("Missing column(s): {glue::collapse(mandatory_columns[!check_columns], sep = ', ')}"), call. = FALSE)
  
  .df <- .df %>%
    filter(!grepl(glue::glue("^{.ignore}$"), id, ignore.case = TRUE)) %>%
    bind_cols(.group = group_indices(.)) %>%
    group_by(.group) %>%
    nest() %>% 
    mutate(std = map(data, get_standard),
           model = map(std, safe_drm_lite, rlang::UQE(.od) ~ log10(.dose),
                       fct = drc::LL.4(names = c("Slope", "Lower", "Upper", "ED50")),
                       logDose = 10))  %>%
    mutate(model = at_depth(model, 2, list),
           model = map(model, as_tibble),
           model = map(model, set_names, c("drm", "drm_error"))) %>%
    unnest(model)
  
  # Check if an error occured during a regression
  if (.df %>% filter(!map_lgl(drm_error, is_empty)) %>% nrow() > 0) {
    warning("At least one error occured during the regression", call. = FALSE)
    .df %>%
      filter(!map_lgl(drm_error, is_empty)) %>%
      mutate(error_msg = map_chr(drm_error, "message")) %>%
      group_by(.group, error_msg) %>%
      summarise() %>%
      deframe() %>%
      walk2(names(.), ~warning(glue::glue("{.x} (in group {.y})"), call. = FALSE))
  }
  
  # Using estimate (customised drc::ED call) to compute the estimated concentration.
  .df <- .df %>%
    mutate(estimate = map2(map(data, quo_name(.od)), drm, quietly(estimate), type = "absolute", display = FALSE),
           estimate = at_depth(estimate, 2, list),
           estimate = map(estimate, as_tibble)) %>%
    unnest(estimate) %>%
    mutate(warnings = map(warnings, unique)) %>%
    unnest(data, result)
  
  # Handling dilution
  if (!rlang::quo_is_null(.dilution)) {
    .df <- .df %>%
      mutate(!!quo_name(.dilution) := replace(!!.dilution, is.na(!!.dilution), 1),
             estimate = estimate * !!.dilution,
             std_error = std_error * !!.dilution)
  }
  
  .df
  
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
    mutate(.valid = ifelse(.y <= max(.y[grepl(paste0("^", std.key), ignore.case = TRUE, id)], na.rm = TRUE) & !is.na(.y), TRUE, FALSE)) %>%
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
