#' @importFrom rlang current_env
#' @importFrom data.table setorder uniqueN data.table fwrite
#' @importFrom purrr map2
#' 
#' @noRd  
#' @keywords internal
.write_shock <- function(shock,
                         write_path) {
  UseMethod(".write_shock")
}

#' @export
.write_shock.uniform <- function(shock,
                                 write_path) {
  cat(shock$shock,
    file = write_path,
    sep = "\n",
    append = TRUE
  )
  return(write_path)
}

#' @export
.write_shock.user <- function(shock,
                              write_path) {

  writeLines(
    shock,
    write_path
  )

  return(write_path)
}

#' @export
.write_shock.custom <- function(shock,
                                write_path) {
  ndigits <- .o_ndigits()
  list2env(shock, rlang::current_env())

  dt[, Value := format(
    round(Value, ndigits),
    trim = TRUE,
    nsmall = ndigits,
    scientific = FALSE
  )]

  data.table::setorder(dt)

  # column names for the algo
  if (!colnames(dt) %=% "Value") {
    mixed_names <- setdiff(colnames(dt), "Value")
    std_names <- ifelse(grepl("\"", mixed_names),
      mixed_names,
      .dock_tail(string = mixed_names)
    )
  } else {
    std_names <- "Value"
  }

  # partial var condition
  if (!attr(shock, "full_var")) {
    shock_ele <- dt[, lapply(.SD, function(r) {
      paste0("\"", r, "\"")
    }), .SDcols = mixed_names]

    writeout <- paste("Shock", paste0(
      var_name,
      "(",
      apply(
        shock_ele,
        MARGIN = 1,
        paste0,
        collapse = ","
      ),
      ")"
    ), "=", paste0(dt[, "Value"][[1]], ";\n"))

    cat(writeout,
      file = write_path,
      sep = "",
      append = TRUE
    )
  } else if (attr(shock, "full_var")) {
    if (idx %=% 0L) {
      stop("No column names other than Value detected. This would indicate
               that a shock type 'uniform' is appropriate.")
    } else if (idx %=% 1L) {
      # lead
      lead <- paste("Shock", paste0(var_name, "(", std_names, ")"), "=")

      cat(lead,
        "\n",
        file = write_path,
        append = TRUE
      )
      # vector
      arr <- as.array(dt[[2]])
      cat(arr,
        file = write_path,
        sep = " ",
        append = TRUE
      )
      cat("\n", ";", "\n",
        file = write_path,
        sep = "",
        append = TRUE
      )
    } else if (idx %=% 2L) {
      lead <- paste(
        "Shock",
        paste0(var_name, "(", paste0(std_names, collapse = ","), ")"),
        "="
      )

      cat(lead, "\n", file = write_path, append = TRUE)
      # matrix
      unique_dim1 <- unique(dt[[2]])
      unique_dim2 <- unique(dt[[1]])
      mat <- matrix(dt[[3]], nrow = length(unique_dim1), byrow = FALSE)
      for (row in seq_len(nrow(mat))) {
        cat(mat[row, ],
          file = write_path,
          sep = " ",
          append = TRUE
        )

        cat("\n",
          file = write_path,
          append = TRUE
        )
      }
      cat(";", "\n",
        file = write_path,
        sep = "",
        append = TRUE
      )
    } else {
      dim_sizes <- rev(sapply(
        X = 1:(idx),
        FUN = function(i) {
          data.table::uniqueN(dt[[i]])
        }
      ))
      arr <- array(dt[[idx + 1]], dim_sizes)
      ele_table <- unique(dt[, seq_len(length.out = idx - 2), with = FALSE])
      ele_table <- ele_table[, lapply(.SD, function(r) {
        paste0("\"", r, "\"")
      })]
      full_sets <- std_names[c(idx - 1, idx)]
      full_sets <- data.table::data.table(rep(full_sets[1], nrow(ele_table)), rep(full_sets[2], nrow(ele_table)))
      lead_table <- cbind(ele_table, full_sets)
      var_name <- strsplit(var_name, "\\(")[[1]][1]
      leads <- paste("Shock", paste0(
        var_name,
        "(",
        apply(
          lead_table,
          MARGIN = 1,
          paste0,
          collapse = ","
        ),
        ")"
      ), "=")

      ls_dt <- .slice_array(
        arr = arr,
        dim_sizes = dim_sizes,
        n_dims = idx
      )

      purrr::map2(
        .x = leads,
        .y = ls_dt,
        .f = function(l, d) {
          cat(l, "\n",
            file = write_path,
            sep = "",
            append = TRUE
          )

          data.table::fwrite(
            d,
            file = write_path,
            sep = " ",
            col.names = FALSE,
            append = TRUE
          )

          cat(";", "\n", "\n",
            file = write_path,
            sep = "",
            append = TRUE
          )
        }
      )
    }
  }
  return(write_path)
}