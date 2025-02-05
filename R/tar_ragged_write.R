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
#' @importFrom data.table fwrite setorder dcast.data.table uniqueN
#' @return The write_path of the written file.
#' @keywords internal
#' @noRd
.ragged_write <- function(dat,
                          ndigits,
                          file_name,
                          out_dir) {
  browser()
  # write to
  write_path <- file.path(out_dir, file_name)

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

      if (!identical(x = idx, y = 0L)) {
      # consistency check between lead (constructed from sets) and actual data
      dim_sizes <- sapply(
        X = 1:(idx),
        FUN = function(i) {
          data.table::uniqueN(dt[[i]])
        }
      )

      lead_dim <- as.integer(x = strsplit(x = trimws(x = sapply(
        X = strsplit(x = lead, split = "Real|Integer"), "[[", 1
      )), "\\s")[[1]])

      if (!identical(x = dim_sizes, y = lead_dim)) {
        stop(
          cat(
            "Lead set dim and data dim mismatch detected.\nLead dim:",
            lead,
            "\nData dim:",
            dim_sizes
          )
        )
      }
      }

      # write the header lead with attributes
      cat(lead,
        file = write_path,
        sep = "\n",
        append = TRUE
      )

    # algo for converting to the ragged edge style
    if (identical(x = idx, y = 0L)) {
      data.table::fwrite(
        x = dt,
        file = write_path,
        quote = FALSE,
        append = TRUE
      )

    } else if (identical(x = idx, y = 1L)) {
      data.table::setorder(dt)
      data.table::fwrite(
        x = dt[, -1],
        file = write_path,
        quote = FALSE,
        append = TRUE
      )} else {
        data.table::setcolorder(x = dt, neworder = rev(x = colnames(x = dt[, !"Value"])))
        data.table::setorder(x = dt)

        arr <- array(data = dt[[idx + 1]], dim = unlist(x = dim_sizes))

        if (identical(x = idx, y = 2L)) {
          data.table::fwrite(
            x = as.data.table(arr),
            file = write_path,
            sep = ",",
            col.names = FALSE,
            append = TRUE
          )
        } else {
          # dynamic indexing for >= 3 dim
          ls_dt <- .slice_array(arr = arr,
                                dim_sizes = dim_sizes,
                                n_dim = idx)

          lapply(
            X = ls_dt,
            FUN = function(d) {
              data.table::fwrite(
                x = d,
                file = write_path,
                sep = ",",
                col.names = FALSE,
                append = TRUE
              )

              cat("\n",
                  file = write_path,
                  sep = "",
                  append = TRUE)
            }
          )
        }
      }
      if (idx < 3) {
        cat("\n",
            file = write_path,
            sep = "",
            append = TRUE
        )
      }
    }
  )

  return(write_path)
}
