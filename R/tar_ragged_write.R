#' Write Data to File
#'
#' This function writes data to a file in a specific format based on the 'idx'
#' parameter.
#'
#' @inheritParams teems_model
#'
#' @param dat A list containing data.tables to be written to file.
#' @param file_name A string specifying the name of the output file.
#' @param out_dir A string specifying the directory where the output file will
#'   be written.
#'
#' @importFrom purrr pmap
#' @importFrom data.table fwrite setorder dcast.data.table
#' @return The path of the written file.
#' @keywords internal
#' @noRd
.ragged_write <- function(dat,
                          ndigits,
                          file_name,
                          out_dir) {
  # extend to 7 dimensions which is current GEMPack limit for Real arrays.
  # this could probably be rewritten with Rcpp

  # write to
  path <- file.path(out_dir, file_name)

  purrr::pmap(
    .l = list(
      dat[["dt"]],
      dat[["lead"]],
      dat[["idx"]]
    ),
    .f = function(dt, lead, idx) {
      # column names for the algo
      if (!identical(x = colnames(x = dt), y = "Value")) {
        dtColnames <- setdiff(x = colnames(x = dt), y = "Value")
      } else {
        dtColnames <- "Value"
      }

      if (!is.integer(dt[["Value"]])) {
      # format values
      dt[, Value := format(
        round(Value, ndigits),
        trim = TRUE,
        nsmall = ndigits,
        scientific = FALSE
      )]
      }

      # write the header lead with attributes
      cat(lead,
        file = path,
        sep = "\n",
        append = TRUE
      )

      # algo for converting to the ragged edge style
      if (identical(x = idx, y = as.integer(x = 0))) {
        data.table::fwrite(
          x = dt,
          file = path,
          quote = FALSE,
          append = TRUE
        )

        # separator between header/data sets
        cat("\n",
          file = path,
          sep = "",
          append = TRUE
        )
      } else if (identical(x = idx, y = as.integer(x = 1))) {
        data.table::setorder(dt)

        data.table::fwrite(
          x = dt[, -1],
          file = path,
          quote = FALSE,
          append = TRUE
        )

        # separator between header/data sets
        cat("\n",
          file = path,
          sep = "",
          append = TRUE
        )
      } else if (identical(x = idx, y = as.integer(x = 2))) {
        dt <- data.table::dcast.data.table(dt,
          formula = paste(dtColnames, collapse = "~"),
          value.var = "Value"
        )

        data.table::fwrite(
          x = dt[, -1],
          file = path,
          quote = FALSE,
          append = TRUE
        )

        # separator between header/data sets
        cat("\n",
          file = path,
          sep = "",
          append = TRUE
        )
      } else if (identical(x = idx, y = as.integer(x = 3))) {
        dt <- data.table::dcast.data.table(dt,
          formula = get(x = dtColnames[3]) + get(x = dtColnames[1]) ~ get(x = dtColnames[2]),
          value.var = "Value"
        )

        dt_ls <- split(x = dt, by = "dtColnames")

        lapply(X = dt_ls, FUN = function(d) {
          data.table::fwrite(
            x = d[, -(1:2)],
            file = path,
            quote = FALSE,
            append = TRUE
          )

          cat("\n", file = path, sep = "", append = TRUE)
        })
      } else if (identical(x = idx, y = as.integer(x = 4))) {
        dt <- data.table::dcast.data.table(
          data = dt,
          formula = get(x = dtColnames[4]) + get(x = dtColnames[3]) + get(x = dtColnames[1]) ~ get(x = dtColnames[2]),
          value.var = "Value"
        )

        dt_ls <- split(x = dt, by = c("dtColnames_1", "dtColnames"))

        lapply(X = dt_ls, FUN = function(d) {
          data.table::fwrite(
            x = d[, -(1:3)],
            file = path,
            quote = FALSE,
            append = TRUE
          )

          cat("\n", file = path, sep = "", append = TRUE)
        })
      } else if (identical(x = idx, y = as.integer(x = 5))) {
        dt <- dcast.data.table(
          data = dt,
          formula = get(x = dtColnames[5]) + get(x = dtColnames[3]) + get(x = dtColnames[4]) + get(x = dtColnames[1]) ~ get(x = dtColnames[2]),
          value.var = "Value"
        )

        dt_ls <- split(x = dt, by = c("dtColnames", "dtColnames_1", "dtColnames_2"))

        lapply(X = dt_ls, FUN = function(d) {
          data.table::fwrite(
            x = d[, -(1:4)],
            file = path,
            quote = FALSE,
            append = TRUE
          )

          cat("\n", file = path, sep = "", append = TRUE)
        })
      } else if (idx > 5) {
        stop("Contact package author for extension of write algo.")
      }
    }
  )

  return(path)
}
