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
#' @importFrom data.table fwrite setorder dcast.data.table
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

  # refactoring needed
  # column names for the algo
  if (!identical(x = colnames(x = dt), y = "Value")) {
    dtColnames_idx <- setdiff(x = colnames(x = dt), y = "Value")
    dtColnames <- substr(
      x = dtColnames_idx,
      start = 1,
      stop = nchar(dtColnames_idx) - 1
    )
  } else {
    dtColnames <- "Value"
  }

  # algo for converting to the ragged edge style
  if (identical(x = idx, y = as.integer(x = 0))) {
    stop("No column names other than Value detected. This would indicate
             that a shock type 'uniform' is appropriate.")
  } else if (identical(x = idx, y = as.integer(x = 1))) {
    if (attr(x = var_name, which = "full_var")) {
      cat(paste("Shock", paste0(var_name, "(", dtColnames, ")"), "="),
          "\n",
          file = write_path,
          append = TRUE)
    } else {
      cat(paste("Shock", var_name, "="),
          "\n",
          file = write_path,
          append = TRUE)
    }

    data.table::setorder(dt)

    cat(unlist(x = dt[, -1]),
      file = write_path,
      sep = " ",
      append = TRUE
    )

    # seperator between header/data sets
    cat("\n", ";", "\n", "\n",
      file = write_path,
      sep = "",
      append = TRUE
    )
  } else if (identical(x = idx, y = as.integer(x = 2))) {
    ls_dt <- split(x = dt, by = dtColnames_idx[[1]])
    purrr::map2(
      .x = ls_dt,
      .y = names(x = ls_dt),
      .f = function(dt_ele, ele) {
        # lead
        if (attr(x = var_name, which = "full_var")) {
        cat(
          paste("Shock", paste0(
            var_name,
            "(",
            paste0("\"", ele, "\"", ",", dtColnames[[2]]),
            ")"
          ), "="), "\n",
          file = write_path,
          append = TRUE
        )
        } else {
          pattern <- paste0(paste0(dtColnames, collapse = ","),")")
          var_name <- sub(pattern = pattern, replacement = "", x = var_name)
          cat(
            paste("Shock", paste0(
              var_name,
              paste0("\"", ele, "\"", ",", dtColnames[[2]]),
              ")"
            ), "="), "\n",
            file = write_path,
            append = TRUE
          )
        }

        # data
        cat(unlist(x = dt_ele[, 3]),
          file = write_path,
          sep = " ",
          append = TRUE
        )

        cat("\n", ";", "\n", "\n",
          file = write_path,
          sep = "",
          append = TRUE
        )
      }
    )
  } else if (identical(x = idx, y = as.integer(x = 3))) {
    ls_dt <- split(x = dt, by = dtColnames_idx[[1]])

    purrr::map2(
      .x = ls_dt,
      .y = names(x = ls_dt),
      .f = function(dt_ele, ele) {
        # lead
        if (attr(x = var_name, which = "full_var")) {
        cat(
          paste("Shock", paste0(
            var_name,
            "(",
            paste0("\"", ele, "\"", ",", paste(dtColnames[2:3], collapse = ",")),
            ")"
          ), "="), "\n",
          file = write_path,
          append = TRUE
        )
        } else {
          pattern <- paste0(paste0(dtColnames, collapse = ","),")")
          var_name <- sub(pattern = pattern, replacement = "", x = var_name)
          cat(
            paste("Shock", paste0(
              var_name,
              paste0("\"", ele, "\"", ",", paste0(dtColnames[2:3], collapse = ",")),
              ")"
            ), "="), "\n",
            file = write_path,
            append = TRUE
          )
        }

        dt_ele <- data.table::dcast.data.table(
          data = dt_ele,
          formula = paste(rev(x = dtColnames_idx[2:3]), collapse = "~"),
          value.var = "Value"
        )
        # data
        data.table::fwrite(
          x = dt_ele[, -1],
          file = write_path,
          append = TRUE,
          quote = FALSE,
          sep = " "
        )

        cat(";", "\n", "\n",
          file = write_path,
          sep = "",
          append = TRUE
        )
      }
    )
  } else if (identical(x = idx, y = as.integer(x = 4))) {
    ls_dt <- split(x = dt, by = dtColnames_idx[1:2])

    purrr::map2(
      .x = ls_dt,
      .y = names(x = ls_dt),
      .f = function(dt_ele, ele) {
        # multiset ele fix
        ele <- paste0("\"",
          unlist(x = strsplit(x = ele, split = "\\.")),
          "\"",
          collapse = ","
        )

        # lead
        if (attr(x = var_name, which = "full_var")) {
        cat(
          paste("Shock", paste0(
            var_name,
            "(",
            paste0(ele, ",", paste(dtColnames[3:4], collapse = ",")),
            ")"
          ), "="), "\n",
          file = write_path,
          append = TRUE
        )
        } else {
          stop("not working yet")
          pattern <- paste0(paste0(dtColnames, collapse = ","),")")
          var_name <- sub(pattern = pattern, replacement = "", x = var_name)
          cat(
            paste("Shock", paste0(
              var_name,
              paste0("\"", ele, "\"", ",", paste0(dtColnames[3:4], collapse = ",")),
              ")"
            ), "="), "\n",
            file = write_path,
            append = TRUE
          )
        }

        dt_ele <- data.table::dcast.data.table(
          data = dt_ele,
          formula = paste(rev(x = dtColnames_idx[3:4]), collapse = "~"),
          value.var = "Value"
        )
        # data
        data.table::fwrite(
          x = dt_ele[, -1],
          file = write_path,
          append = TRUE,
          quote = FALSE,
          sep = " "
        )

        cat(";", "\n", "\n",
          file = write_path,
          sep = "",
          append = TRUE
        )
      }
    )
  } else if (identical(x = idx, y = as.integer(x = 5))) {
    ls_dt <- split(x = dt, by = dtColnames_idx[1:3])

    purrr::map2(
      .x = ls_dt,
      .y = names(x = ls_dt),
      .f = function(dt_ele, ele) {
        # multiset ele fix
        ele <- paste0("\"",
          unlist(x = strsplit(x = ele, split = "\\.")),
          "\"",
          collapse = ","
        )

        # lead
        cat(
          paste("Shock", paste0(
            var_name,
            "(",
            paste0(ele, ",", paste(dtColnames[4:5], collapse = ",")),
            ")"
          ), "="), "\n",
          file = write_path,
          append = TRUE
        )

        dt_ele <- data.table::dcast.data.table(
          data = dt_ele,
          formula = paste(rev(x = dtColnames_idx[4:5]), collapse = "~"),
          value.var = "Value"
        )
        # data
        data.table::fwrite(
          x = dt_ele[, -1],
          file = write_path,
          append = TRUE,
          quote = FALSE,
          sep = " "
        )

        cat(";", "\n", "\n",
          file = write_path,
          sep = "",
          append = TRUE
        )
      }
    )
  } else if (identical(x = idx, y = as.integer(x = 5))) {
    stop("Get dev to extend aux_write_ragged_shk algo")
  }
  return(write_path)
}
