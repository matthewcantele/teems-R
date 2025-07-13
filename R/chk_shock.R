#' @importFrom purrr pluck
#' @importFrom data.table fread fwrite setcolorder
#'
#' @keywords internal
.check_shock <- function(shock,
                         var_extract,
                         int_sets = NULL,
                         call) {

  shock <- .validate_shock(shock = shock, call = call)

  if (!shock$var %in% var_extract$name) {
    var_name <- shock$var
    .cli_action(
      shk_err$not_a_var,
      action = "abort",
      call = call
    )
  }

  UseMethod(".check_shock", shock)
}

#' @export
.check_shock.uniform <- function(shock,
                                 var_extract,
                                 int_sets = NULL,
                                 call) {
  shock <- .validate_shock(shock = shock, call = call)
  ls_mixed <- purrr::pluck(var_extract, "ls_mixed_idx", shock$var)

  if (!shock$subset %=% NA) {
    sets <- names(shock$subset)
    if (!is.null(int_sets)) {
      if (any(grepl(pattern = "Year", sets))) {
        int_sets <- paste0(int_sets, "t")
        if (any(int_sets %in% sets)) {
          supplied_int_set <- intersect(sets, int_sets)
          .cli_action(
            shk_err$extra_col,
            action = "abort",
            url = NULL,
            hyperlink = NULL,
            call = call
          )
        }
        set_check <- c(setdiff(ls_mixed, int_sets), "Year")
      } else {
        set_check <- ls_mixed
      }

      if (!all(sets %in% set_check)) {
        errant_set <- setdiff(sets, set_check)
        l_errant_set <- length(errant_set)
        var_name <- shock$var
        .cli_action(
          shk_err$invalid_set,
          action = c("abort", "inform", "inform", "inform"),
          url = NULL,
          hyperlink = NULL,
          call = call
        )
      }
    }
    attr(shock, "full_var") <- FALSE
  } else {
    attr(shock, "full_var") <- TRUE
  }

  shock$ls_mixed <- ls_mixed
  shock$ls_upper <- purrr::pluck(var_extract, "ls_upper_idx", shock$var)

  return(shock)
}

#' @export
.check_shock.custom <- function(shock,
                                var_extract,
                                int_sets = NULL,
                                call) {
  shock <- .validate_shock(shock = shock, call = call)
  shock <- .chk_cst_scen(
    shock = shock,
    var_extract = var_extract,
    int_sets = int_sets,
    call = call
  )

  return(shock)
}

#' @export
.check_shock.scenario <- function(shock,
                                  var_extract,
                                  int_sets = NULL,
                                  call) {
  shock <- .validate_shock(shock = shock, call = call)
  if (is.null(int_sets)) {
    .cli_action(shk_err$scen_dynamic,
      action = "abort",
      call = call
    )
  }

  shock <- .chk_cst_scen(
    shock = shock,
    var_extract = var_extract,
    int_sets = int_sets,
    call = call
  )
  return(shock)
}
