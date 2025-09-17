#' @importFrom purrr map2 pmap
#' @importFrom cli cli_text cli_alert_success
#'
#' @keywords internal
#' @noRd
.check_baseline <- function(outputs,
                            data,
                            model,
                            max_tolerance,
                            null_shock,
                            call) {
  var <- subset(outputs, type == "variable")
  var_range <- lapply(var$dat, function(dt) {
    range(dt$Value)
  })

  if (null_shock) {
    all_range <- range(var_range)
    if (!all_range %=% c(0, 0)) {
      .cli_action(check_wrn$var_baseline,
        action = "warn",
        call = call
      )
    }
  }
  coeff <- subset(outputs, type == "coefficient")
  model_coeff_headers <- subset(
    model,
    type == "Coefficient" & !is.na(header) & !name %in% c(.o_n_timestep_header(), .o_timestep_header())
  )
  coeff <- subset(
    coeff,
    name %in% model_coeff_headers$name
  )
  r_idx <- match(coeff$name, model$name)
  coeff$header <- model$header[r_idx]
  data <- subset(
    data,
    names(data) %in% model_coeff_headers$header
  )
  if (!all(names(data) %in% coeff$header)) {
    x_coeff <- setdiff(names(data), coeff$header)
    .cli_action(check_err$missing_coeff,
      action = "abort",
      call = call
    )
  }

  r_idx <- match(names(data), model$header)
  mixed_col <- model$ls_mixed_idx[r_idx]

  if (any(grepl("\\(intertemporal\\)", model$qualifier_list))) {
    intertemporal <- TRUE
    int_sets <- subset(model,
      qualifier_list == "(intertemporal)",
      select = name,
      1
    )
    stnd_col <- model$ls_upper_idx[r_idx]
    data <- purrr::pmap(
      list(
        data,
        mixed_col,
        stnd_col
      ),
      function(dt, m_col, s_col) {
        if (any(s_col %in% int_sets)) {
          dt$V1 <- -1
          colnames(dt)[ncol(dt)] <- s_col[s_col %in% int_sets]
          dt <- dt[, c(..s_col, "Value")]
        }
        return(dt)
      }
    )
  }

  data <- purrr::map2(
    data,
    mixed_col,
    function(dt, col) {
      if (!col %=% NA_character_) {
        old_col <- colnames(dt)[!colnames(dt) %in% "Value"]
        colnames(dt) <- c(col, "Value")
      }
      return(dt)
    }
  )

  r_idx <- match(coeff$header, names(data))
  coeff$input_dat <- data[r_idx]

  if (!intertemporal) {
    coeff$dat <- purrr::map2(
      coeff$dat,
      coeff$input_dat,
      merge
    )
  } else {
    coeff$dat <- purrr::map2(
      coeff$dat,
      coeff$input_dat,
      function(o, i) {
        data.table::rbindlist(list(i, o[, !"Year"]))
      }
    )
  }
browser()
  coeff$dat <- purrr::map2(
    coeff$dat,
    coeff$name,
    function(dt, nme) {
      attr(dt, "coeff") <- nme
      return(dt)
    }
  )

  if (!intertemporal) {
    invisible(lapply(coeff$dat, function(dt) {
      if (!colnames(dt) %=% "Value") {
        v_col <- c("Value.x", "Value.y")
        dt$diff <- apply(dt[, v_col, with = FALSE], 1, diff)
        dt$delta <- apply(dt[, as.character(v_col), with = FALSE], 1, function(x) {
          if (x[1] != 0 && x[2] != 0) {
            ((x[2] - x[1]) / x[1]) * 100
          } else {
            return(0)
          }
        })

        d_max <- max(abs(dt$delta))
        delta_max <- sprintf(paste0("%1.6f%%"), d_max)
        text <- "{.field {attr(dt, 'coeff')}}: Max delta {delta_max}"
        if (abs(d_max) < max_tolerance) {
          cli::cli_text(text)
        } else {
          .cli_action(text,
            action = "abort",
            call = call
          )
        }
      }
    }))
  } else {
    invisible(lapply(coeff$dat, function(dt) {
      if ("ALLTIMEt" %in% colnames(dt)) {
        browser()
        coeff <- attr(dt, "coeff")
        y_col <- range(dt$ALLTIMEt)
        dt <- dt[ALLTIMEt %in% y_col]
        dt <- data.table::dcast.data.table(dt,
          formula = ... ~ ALLTIMEt,
          value.var = "Value"
        )
        dt[, delta := apply(dt[, as.character(y_col), with = FALSE], 1, function(x) {
          if (x[1] != 0 && x[2] != 0) {
            ((x[2] - x[1]) / x[1]) * 100
          } else {
            return(0)
          }
        })]

        d_max <- max(abs(dt$delta))
        delta_max <- sprintf(paste0("%1.6f%%"), d_max)
        text <- "{.field {coeff}}: Max delta {delta_max}"
        if (abs(d_max) < max_tolerance) {
          cli::cli_text(text)
        } else {
          .cli_action(text,
            action = "abort",
            call = call
          )
        }
      }
    }))
  }

  max_tolerance <- sprintf("%.6f%%", max_tolerance)
  cli::cli_alert_success("Change across all input coefficients within {max_tolerance}")
}