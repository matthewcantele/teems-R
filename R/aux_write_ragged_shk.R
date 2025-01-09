#' Write Data to File
#'
#' This function writes data to a file in a specific format based on the 'idx'
#' parameter.
#'
#' @param dat A list containing data.tables to be written to file.
#' @param write_path A string specifying the name of the output file.
#' @note Extend to 7 dimensions which is current GEMPack limit for Real arrays.
#'   Perhaps Ha can indicate whether the solver is capable of more
#' @note I'm open to alternatives for the algo below. Some simplification
#'   possible at the cost of readability.
#'
#' @importFrom purrr map2
#' @importFrom data.table fwrite setorder dcast.data.table uniqueN
#' @return The path of the written file.
#' @keywords internal
#' @noRd
.ragged_shk_write <- function(dt,
                              ndigits,
                              var_name,
                              idx,
                              write_path) {

  # format values
  dt[, Value := format(
    round(Value, ndigits),
    trim = TRUE,
    nsmall = ndigits,
    scientific = FALSE
  )]

  data.table::setorder(x = dt)

  # column names for the algo
  if (!identical(x = colnames(x = dt), y = "Value")) {
    dtColnames_idx <- setdiff(x = colnames(x = dt), y = "Value")
    dtColnames <- substr(
      x = dtColnames_idx,
      start = 1,
      stop = nchar(x = dtColnames_idx) - 1
    )
  } else {
    dtColnames <- "Value"
  }

    if (identical(x = idx, y = 0L)) {
      stop("No column names other than Value detected. This would indicate
               that a shock type 'uniform' is appropriate.")
    } else  if (identical(x = idx, y = 1L)) {
    # lead
    if (attr(x = var_name, which = "full_var")) {
      lead <- paste("Shock", paste0(var_name, "(", dtColnames, ")"), "=")
    } else {
      stop("A partial shock on a variable with only one set should be handled with shock type 'uniform' and direct element specification.")
    }
    cat(lead,
        "\n",
        file = write_path,
        append = TRUE)
    # vector
    arr <- as.array(dt[[2]])
    cat(arr,
        file = write_path,
        sep = " ",
        append = TRUE)
    cat("\n", ";", "\n",
        file = write_path,
        sep = "",
        append = TRUE)
  } else if (identical(x = idx, y = 2L)) {
    # lead
    if (!attr(x = var_name, which = "full_var")) {
      pattern <- paste0(paste0(dtColnames, collapse = ","), ")")
      var_name <- sub(pattern = pattern,
                      replacement = "",
                      x = var_name)
    }

    lead <- paste("Shock",
                  paste0(var_name, "(", paste0(dtColnames, collapse = ","), ")"),
                  "=")

    cat(lead, "\n", file = write_path, append = TRUE)
    # matrix
    unique_dim1 <- unique(dt[[2]])
    unique_dim2 <- unique(dt[[1]])
    mat <- matrix(dt[[3]], nrow = length(unique_dim1), byrow = FALSE)
    for (row in seq_len(nrow(mat))) {
      cat(mat[row, ],
          file = write_path,
          sep = " ",
          append = TRUE)

      cat("\n",
          file = write_path,
          append = TRUE)
    }
    cat(";", "\n",
        file = write_path,
        sep = "",
        append = TRUE)
  } else {
    # lead
    #if (attr(x = var_name, which = "full_var")) {
    # cat(
    #   paste("Shock", paste0(
    #     var_name,
    #     "(",
    #     paste0("\"", ele, "\"", ",", paste(dtColnames[2:3], collapse = ",")),
    #     ")"
    #   ), "="), "\n",
    #   file = write_path,
    #   append = TRUE
    # )
    # } else {
    #   pattern <- paste0(paste0(dtColnames, collapse = ","),")")
    #   var_name <- sub(pattern = pattern, replacement = "", x = var_name)
    #   cat(
    #     paste("Shock", paste0(
    #       var_name,
    #       paste0("\"", ele, "\"", ",", paste0(dtColnames[2:3], collapse = ",")),
    #       ")"
    #     ), "="), "\n",
    #     file = write_path,
    #     append = TRUE
    #   )
    # }

    dim_sizes <- lapply(
      X = 1:(idx),
      FUN = function(i) {
        data.table::uniqueN(dt[[i]])
      }
    )
    arr <- array(data = dt[[idx + 1]], dim = rev(x = unlist(x = dim_sizes)))
    ele_table <- unique(x = dt[, seq_len(length.out = idx - 2), with = FALSE])
    ele_table <- ele_table[, lapply(.SD, function(r){paste0("\"", r, "\"")})]
    full_sets <- dtColnames[c(idx - 1, idx)]
    full_sets <- data.table::data.table(rep(full_sets[1], nrow(ele_table)), rep(full_sets[2], nrow(ele_table)))
    lead_table <- cbind(ele_table, full_sets)
    leads <- paste("Shock", paste0(
      var_name,
      "(",
      apply(
        X = lead_table,
        MARGIN = 1,
        paste0,
        collapse = ","
      ),
      ")"
    ), "=")


  dims <- dim(arr)
  n_dims <- length(dims)
  other_dims <- setdiff(1:n_dims, c(1,2))
  other_indices <- lapply(X = dims[other_dims],
                          FUN = seq_len)
  other_grid <- expand.grid(other_indices)

  # list of matrices
  ls_mat <- list()
  for (i in 1:nrow(other_grid)) {
    indices <- as.list(other_grid[i,])
    full_indices <- vector("list", n_dims)
    full_indices[c(1,2)] <- list(quote(expr = ))
    full_indices[other_dims] <- indices
    ls_mat[[i]] <- do.call("[", c(list(arr), full_indices))
  }

  ls_dt <- lapply(X = ls_mat,
                  FUN = data.table::as.data.table)

  purrr::map2(.x = leads,
              .y = ls_dt,
              .f = function(l, d) {
                cat(l, "\n",
                    file = write_path,
                    sep = "",
                    append = TRUE)

                data.table::fwrite(x = d,
                                   file = write_path,
                                   sep = " ",
                                   col.names = FALSE,
                                   append = TRUE)

                cat(";", "\n", "\n",
                    file = write_path,
                    sep = "",
                    append = TRUE)

              })
  }
  return(write_path)
}
