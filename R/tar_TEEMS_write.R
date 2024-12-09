#' Write TEEMS to File
#'
#' This is a custom write function. It supports writing via `cat`, or
#' `writeLines`.
#'
#' @param input The TEEMS data to be written.
#' @param file The name of the file to which the TEEMS data will be written.
#' @param write_object Object type to write.
#' @param write_dir The directory where the file will be created.
#'
#' @return The path to the written file.
#' @keywords internal
#' @noRd
.TEEMS_write <- function(input,
                         ndigits,
                         file,
                         write_object,
                         write_dir) {

  # Check if the write_dir exist
  if (!dir.exists(paths = write_dir)) {
    stop(paste("The write directory", dQuote(x = write_dir), "does not exist."))
  }
  # full write path
  write_path <- file.path(write_dir, file)

  if (is.element(el = write_object, set = c("cmf"))) {
    # write
    cat(input,
      file = write_path,
      sep = "\n")
    } else if (identical(x = write_object, y = "shk")) {
      if (!is.null(x = input)) {
        for (shk in input) {
          if (identical(x = shk[["type"]], y = "uniform")) {
            cat(shk[["shock"]],
                file = write_path,
                sep = "\n",
                append = TRUE)
          } else if (identical(x = shk[["type"]], y = "custom")) {
            .ragged_shk_write(
              dt = shk[["dt"]],
              ndigits = ndigits,
              var_name = shk[["var_name"]],
              idx = shk[["idx"]],
              write_path = write_path
            )
          } else if (identical(x = shk[["type"]], y = "user")) {
            write_path <- file.path(write_dir, basename(path = file))
            writeLines(text = shk[["shock"]],
                       con = write_path)
          }
        }
      } else {
        cat("Shock ;",
            file = write_path,
            sep = "\n",
            append = FALSE)
      }
  } else if (is.element(el = write_object, set = c("cls", "tab"))) {
    writeLines(
      text = input,
      con = write_path
    )
  }

  return(write_path)
}
