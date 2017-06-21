#' @import digest
#' @import readr
#' @import readxl
#' @import dplyr
#' @importFrom purrr map map_lgl map_dbl map_if walk map2 pmap at_depth safely quietly set_names flatten_lgl flatten_chr transpose is_empty is_null
#' @importFrom tidyr gather
#' @import broom

# Adding a "glance" method for drc (see broom)
#' @export
glance.drc <- function(x, ...) {
  x[["coefficients"]] %>%
    set_names(names(.) %>% gsub(":\\(Intercept\\)", "", .)) %>%
    t() %>%
    as_tibble()
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
  
  mandatory_columns <- c(quo_name(.od), "id")

  check_columns <- mandatory_columns %in% colnames(.df)
  if (!all(check_columns)) stop(glue::glue("Missing column(s): {glue::collapse(mandatory_columns[!check_columns], sep = ', ')}"), call. = FALSE)
  
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
  if (is.null(drm)) return(tibble(estimate = rep(NA_real_, length(data)), std_error = estimate))
  drc::ED(drm, data, ...) %>%
    as_tibble() %>%
    set_names(c("estimate", "estimate_sd"))
}

#' @export
elisa_analyse <- function(.df, .od = value, .ignore = c("empty"), dilution = NULL) {
  
  if (!is.data.frame(.df)) stop(".df is not a data.frame")
  
  g <- .df %>%
    groups()
  
  .od <- enquo(.od)
  dilution <- enquo(dilution)
  
  #check_quosures <- c(.od, .dilution)
  #if (!all(check_quosures %>% map_lgl(rlang::quo_is_symbolic) | check_quosures %>% map_lgl(rlang::quo_is_null))) stop("")
  
  mandatory_columns <- c(quo_name(.od), "id")
  if (!is_null(rlang::get_expr(dilution))) {
    mandatory_columns <- c(mandatory_columns, quo_name(dilution))
  }
  
  check_columns <- mandatory_columns %in% colnames(.df)
  if (!all(check_columns)) stop(glue::glue("Missing column(s): {glue::collapse(mandatory_columns[!check_columns], sep = ', ')}"), call. = FALSE)

  .df <- .df %>%
    filter(!grepl(glue::glue("^{.ignore}$"), id, ignore.case = TRUE)) %>%
    bind_cols(.group = group_indices(.)) %>%
    group_by(.group) %>%
    nest() %>% 
    mutate(std = map(data, get_standard, .od = !!.od),
           out_of_range = map2(data, std, ~if_else(max(.y[quo_name(.od)]) < .x[quo_name(.od)], TRUE, FALSE)),
           model = map(std, safe_drm_lite, rlang::UQE(.od) ~ log10(.dose),
                       fct = drc::LL.4(names = c("drm_slope", "drm_lower", "drm_upper", "drm_ed50")),
                       logDose = 10),
           model = at_depth(model, 2, list),
           model = map(model, as_tibble),
           model = map(model, set_names, c("drm", "drm_error"))) %>%
    unnest(model) %>% 
    mutate(params = map(drm, broom::glance)) %>%
    unnest(params)
  
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
    unnest(data, result, out_of_range) %>%
    mutate(estimate_sd = replace(estimate_sd, is.na(estimate) & (!!.od) < drm_lower, NA_real_),
           estimate = replace(estimate, is.na(estimate) & (!!.od) < drm_lower, 0))
  
  # Handling dilution
  if (!rlang::quo_is_null(dilution)) {
    .df <- .df %>%
      mutate(!!quo_name(dilution) := replace(!!dilution, is.na(!!dilution), 1),
             estimate = estimate * !!dilution,
             estimate_sd = estimate_sd * !!dilution)
  }
  
  .df %>%
    group_by(!!!g)
  
}

#' @rdname elisa_analyse
#' @export
elisa_analyze <- elisa_analyse
