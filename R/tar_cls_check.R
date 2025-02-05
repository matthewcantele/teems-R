#' Check Closure
#'
#' This function checks a closure.
#'
#' @inheritParams teems_model
#' @param closure Character vector. File with model-specific list of exogenous
#'   variables.
#' @param sets A list containing set definitions and their elements.
#'
#' @importFrom tibble tibble
#' @importFrom purrr pluck pmap
#' @return A tibble containing the original closure, variable names, and logical
#'   classification of entry type
#' @keywords internal
#' @noRd
.check_closure <- function(closure_file,
                           sets,
                           tab_file) {
  
  closure <- tail(head(readLines(con = closure_file), -3), -1)
  # expand standard closure to possible set and element combinations
  exogenous <- tibble::tibble(orig_closure = closure)

  # isolate all variable names without set or element designation
  exogenous[["var_name"]] <- sapply(
    X = exogenous[["orig_closure"]],
    FUN = function(var) {
      # exogenous by set (not full, not element-specific)
      if (!grepl(pattern = "\\(", x = var)) {
        return(var)
      } else {
        var_name <- purrr::pluck(.x = strsplit(x = var, split = "\\("), 1, 1)
        return(var_name)
      }
    }
  )

  # identify partial set variables
  exogenous[["full_var"]] <- sapply(
    X = exogenous[["orig_closure"]],
    FUN = function(var) {
      # exogenous by set (not full, not element-specific)
      if (!grepl(pattern = "\\(", x = var) && !grepl(pattern = "\"", x = var)) {
        return(var)
      } else {
        return(NA)
      }
    }
  )

  # identify subset variables
  exogenous[["subset_var"]] <- sapply(
    X = exogenous[["orig_closure"]],
    FUN = function(var) {
      # exogenous by set (not full, not element-specific)
      if (grepl(pattern = "\\(", x = var) && !grepl(pattern = "\"", x = var)) {
        return(var)
      } else {
        return(NA)
      }
    }
  )

  # identify partial set - individual element combinations
  exogenous[["mixed_var"]] <- sapply(
    X = exogenous[["orig_closure"]],
    FUN = function(var) {
      # exogenous by set (not full, not element-specific)
      if (grepl(pattern = "\"", x = var) &&
        any(grepl(pattern = paste(toupper(x = sets[["name"]]), collapse = "|"), x = var))) {
        return(var)
      } else {
        return(NA)
      }
    }
  )

  # identify pure element combinations
  exogenous[["ele_var"]] <- sapply(
    X = exogenous[["orig_closure"]],
    FUN = function(var) {
      # exogenous by set (not full, not element-specific)
      if (grepl(pattern = "\"", x = var) &&
        !any(grepl(pattern = paste(toupper(x = sets[["name"]]), collapse = "|"), x = var))) {
        return(var)
      } else {
        return(NA)
      }
    }
  )

  # check that all closure entries have been accounted for
  # check that no inconsistent entries have been used
  # Note: future provide closure var breakdown (after swap)
  if (!all(unlist(x = purrr::pmap(
    .l = list(
      exogenous[["full_var"]],
      exogenous[["subset_var"]],
      exogenous[["mixed_var"]],
      exogenous[["ele_var"]]
    ),
    .f = function(full, set, mixed, ele) {
      if (!is.na(full) && any(!is.na(set), !is.na(mixed), !is.na(ele))) {
        stop(paste(full, "has been designated as both exogenous across all
                   associated elements and in an additional capacity"))
      }
      return(any(is.na(full), is.na(set), is.na(mixed), is.na(ele)))
    }
  )))) {
    stop("One or more closure entries has not been sorted properly.")
  }

  return(exogenous)
}
