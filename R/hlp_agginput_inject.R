#' @importFrom data.table fread fsetequal fsetdiff setcolorder
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.inject_agg_input <- function(tib_data,
                              aggregated_input) {
  for (header in seq_along(aggregated_input)) {
    dat <- aggregated_input[header]
    nme <- names(dat)

    if (!nme %in% tib_data$header) {
      existing_headers <- tib_data$header
      error_fun <- substitute(.cli_action(
        load_err$invalid_input,
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
    existing_dat <- purrr::pluck(tib_data, "dt", nme)

    if (!all(colnames(existing_dat) %in% colnames(dat))) {
      req_col <- colnames(existing_dat)
      error_fun <- substitute(.cli_action(
        load_err$unagg_missing_col,
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

    if (ncol(existing_dat) > 1) {
      if (!data.table::fsetequal(existing_dat[, !"Value"], dat[, !"Value"])) {
        missing_tuples <- data.table::fsetdiff(existing_dat[, !"Value"], dat[, !"Value"])
        n_missing_tuples <- nrow(missing_tuples)
        nme <- nme
        missing_tuples <- capture.output(print(missing_tuples))[-c(1, 2)]
        error_fun <- substitute(.cli_action(
          load_err$unagg_missing_tup,
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
    }
    purrr::pluck(.x = tib_data, "dt", nme) <- dat
  }

  return(tib_data)
}