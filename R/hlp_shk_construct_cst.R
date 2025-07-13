#' @importFrom data.table fread setnames CJ fsetequal fsetdiff rbindlist
#' @importFrom purrr pluck list_flatten
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' 
#' @noRd
#' @keywords internal
#' @export
.construct_shock.custom <- function(raw_shock,
                                    closure,
                                    sets) {
  if (inherits(raw_shock$input, "character")) {
    value <- data.table::fread(raw_shock$input)
  } else {
    value <- raw_shock$input
  }
  
  if ("Year" %in% raw_shock$set) {
    time_set_upper <- intersect(raw_shock$ls_upper, subset(sets, intertemporal, name)[[1]])
    time_set <- raw_shock$ls_mixed[match(time_set_upper, raw_shock$ls_upper)]
    CYRS <- attr(sets, "CYRS")
    if (!all(value$Year %in% CYRS)) {
      errant_year_tuples <- value[!value$Year %in% CYRS]
      n_errant_year_tuples <- nrow(errant_year_tuples)
      errant_year_tuples <- capture.output(print(errant_year_tuples))[-c(1, 2)]
      error_fun <- substitute(.cli_action(
        shk_err$cust_invalid_year,
        action = "abort",
        call = call
      ))

      error_var <- substitute(variables <- list(
        errant_year_tuples = errant_year_tuples,
        n_errant_year_tuples = n_errant_year_tuples
      ))

      error_inputs <- .pipeline_error(
        error_var = error_var,
        error_fun = error_fun,
        call_id = attr(raw_shock, "call_id")
      )

      rlang::abort(error_inputs)
    }
    time_set_ele <- purrr::pluck(sets, "mapped_ele", time_set_upper)
    CYRS <- tibble::tibble(
      YEAR = CYRS,
      time_set_ele
    )
    colnames(CYRS)[2] <- time_set
    r_idx <- match(value$Year, CYRS$YEAR)
    value$Year <- purrr::pluck(CYRS, time_set, r_idx)
    data.table::setnames(value, "Year", time_set)
    raw_shock$set[match("Year", raw_shock$set)] <- time_set
  }

  set_ele <- with(sets$mapped_ele, mget(raw_shock$ls_upper))
  template_shk <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
  data.table::setnames(template_shk, new = raw_shock$ls_mixed)

  if (data.table::fsetequal(template_shk, value[, !"Value"])) {
    attr(raw_shock, "full_var") <- TRUE

    if (.o_check_shock_status()) {
      if (!raw_shock$var %in% closure$full_var) {
        var_name <- raw_shock$var
        error_fun <- substitute(.cli_action(
          shk_err$x_full_exo,
          action = "abort",
          call = call
        ))

        error_var <- substitute(variables <- list(var_name = var_name))
        error_inputs <- .pipeline_error(
          error_var = error_var,
          error_fun = error_fun,
          call_id = attr(raw_shock, "call_id")
        )

        rlang::abort(error_inputs)
      }
    }

    shock <- list(
      dt = value,
      var_name = raw_shock$var,
      idx = .get_index(dt = value)
    )

    shock <- structure(shock,
      class = class(raw_shock),
      full_var = attr(raw_shock, "full_var")
    )

    shock <- list(shock)
  } else {
    attr(raw_shock$var, "full_var") <- FALSE

    if (!nrow(data.table::fsetdiff(value[, !"Value"], template_shk)) %=% 0L) {
      errant_tuples <- data.table::fsetdiff(value[, !"Value"], template_shk)
      errant_tuples <- capture.output(print(errant_tuples))
      errant_tuples <- errant_tuples[-c(1, 2, 3)]
      error_fun <- substitute(.cli_action(shk_err$cust_invalid_tup,
        action = "abort",
        call = call
      ))

      error_var <- substitute(variables <- list(errant_tuples = errant_tuples))
      error_inputs <- .pipeline_error(
        error_var = error_var,
        error_fun = error_fun,
        call_id = attr(raw_shock, "call_id")
      )

      rlang::abort(error_inputs)
    }

    # Year to time conversion here
    if (.o_check_shock_status()) {
      if (raw_shock$var %in% closure$full_var) {
        exo_parts <- purrr::list_flatten(subset(
          closure,
          var_name %in% raw_shock$var,
          struct
        )[[1]])

        all_exo_parts <- data.table::rbindlist(
          l = exo_parts,
          use.names = FALSE
        )

        key_names <- names(value[, !"Value"])
        data.table::setnames(all_exo_parts, new = key_names)
        if (!nrow(data.table::fsetdiff(value[, !"Value"], all_exo_parts)) %=% 0L) {
          x_exo_parts <- data.table::fsetdiff(
            value[, ..key_names],
            all_exo_parts
          )
          x_exo_parts <- trimws(capture.output(print(x_exo_parts)))
          x_exo_parts <- x_exo_parts[-c(1, 2, 3, length(x_exo_parts))]

          error_fun <- substitute(
            .cli_action(shk_err$cust_endo_tup,
              action = "abort"
            ),
            call = call
          )

          error_var <- substitute(variables <- list(x_exo_parts = x_exo_parts))
          error_inputs <- .pipeline_error(
            error_var = error_var,
            error_fun = error_fun,
            call_id = attr(raw_shock, "call_id")
          )
        }
      }
    }

    set_combn <- list()
    key_cols <- names(set_ele)
    for (size in 1:length(key_cols)) {
      col_combinations <- combn(key_cols,
        m = size,
        simplify = FALSE
      )
      set_combn <- c(set_combn, col_combinations)
    }

    set_combn <- set_combn[-length(set_combn)]

    shock <- list()
    shk_idx <- 1

    for (sel_comb in unique(set_combn)) {
      cycle_ele <- with(
        data = set_ele,
        expr = mget(sel_comb)
      )

      ele_combn <- do.call(data.table::CJ, cycle_ele)
      for (sel_ele in 1:nrow(ele_combn)) {
        ele <- unlist(ele_combn[sel_ele, ])
        col_idx <- match(sel_comb, names(set_ele))
        col <- colnames(value)[col_idx]
        # get template subset
        dynamic_filter <- purrr::map2_chr(col, ele, function(c, e) {
          paste(c, "==", paste0("\"", e, "\""))
        })

        if (length(dynamic_filter) > 1) {
          dynamic_filter <- paste(dynamic_filter, collapse = " & ")
        }

        ss_tmpl <- template_shk[eval(parse(text = dynamic_filter))]
        ss_value <- value[eval(parse(text = dynamic_filter))]

        if (all(ss_tmpl %in% ss_value)) {
          shk <- raw_shock
          attr(shk$var, "full_var") <- TRUE
          shk$value <- ss_value
          value <- data.table::fsetdiff(value, ss_value)
          new_col_nme <- paste0("\"", ele, "\"")
          data.table::setnames(shk$value, col, new_col_nme)
          shock[[shk_idx]] <- list(
            dt = shk$value,
            var_name = shk$var,
            idx = .get_index(dt = shk$value)
          )
          shock[[shk_idx]] <- structure(shock[[shk_idx]],
            class = class(raw_shock),
            full_var = attr(raw_shock, "full_var")
          )
          shk_idx <- shk_idx + 1
        }
      }
    }

    if (isTRUE(nrow(value) > 0)) {
      residual_shocks <- list(
        dt = value,
        var_name = raw_shock$var,
        idx = .get_index(dt = value)
      )

      residual_shocks <- structure(residual_shocks,
        class = class(raw_shock),
        full_var = attr(raw_shock, "full_var")
      )

      shock <- c(shock, list(residual_shocks))
    }
  }
  return(shock)
}