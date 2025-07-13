#' @importFrom data.table fread fsetequal fsetdiff setcolorder
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.inject_unagg_input <- function(ls_data,
                                unaggregated_input) {
  for (header in seq_along(unaggregated_input)) {
    dat <- unaggregated_input[header]
    nme <- names(dat)

    if (!nme %in% names(ls_data)) {
      existing_headers <- names(ls_data)
      error_fun <- substitute(.cli_action(
        dat_err$invalid_input,
        action = "abort",
        call = call
      ))
      error_var <- substitute(variables <- list(
        existing_headers = existing_headers,
        nme = nme
      ))
      error_inputs <- .pipeline_error(
        error_var = error_var,
        error_fun = error_fun,
        call_id = attr(unaggregated_input, "call_id")
      )
    }

    dat <- data.table::fread(dat)
    existing_dat <- purrr::pluck(ls_data, nme, "dt")

    if (!all(colnames(existing_dat) %in% colnames(dat))) {
      req_col <- colnames(existing_dat)
      error_fun <- substitute(.cli_action(
        dat_err$unagg_missing_col,
        action = c("abort", "inform"),
        call = call
      ))

      error_var <- substitute(variables <- list(
        req_col = req_col,
        nme = nme
      ))

      error_inputs <- .pipeline_error(
        error_var = error_var,
        error_fun = error_fun,
        call_id = attr(unaggregated_input, "call_id")
      )

      rlang::abort(error_inputs)
    }

    if (!colnames(existing_dat) %=% colnames(dat)) {
      data.table::setcolorder(dat, colnames(existing_dat))
    }

    if (!data.table::fsetequal(existing_dat[, !"Value"], dat[, !"Value"])) {
      missing_tuples <- data.table::fsetdiff(existing_dat[, !"Value"], dat[, !"Value"])
      n_missing_tuples <- nrow(missing_tuples)
      nme <- nme
      missing_tuples <- capture.output(print(missing_tuples))[-c(1, 2)]
      error_fun <- substitute(.cli_action(
        dat_err$unagg_missing_tup,
        action = "abort",
        call = call
      ))

      error_var <- substitute(variables <- list(
        missing_tuples = missing_tuples,
        n_missing_tuples = n_missing_tuples,
        nme = nme
      ))

      error_inputs <- .pipeline_error(
        error_var = error_var,
        error_fun = error_fun,
        call_id = attr(unaggregated_input, "call_id")
      )

      rlang::abort(error_inputs)
    }
    purrr::pluck(.x = ls_data, nme, "dt") <- dat
  }
  return(ls_data)
}
