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
                         file,
                         prepend_file = NULL,
                         ndigits,
                         write_object,
                         write_dir) {

  if (!is.null(x = prepend_file)) {
    file <- basename(path = file)
    file <- paste(prepend_file, file, sep = "_")
  }
  
  # full write path
  write_path <- file.path(write_dir, file)
  if (is.element(el = write_object, set = c("cmf"))) {
    # write
    cat(input,
      file = write_path,
      sep = "\n")
    } else if (identical(x = write_object, y = "shock")) {
      if (!is.null(x = input)) {
        for (shk in input) {
          if (identical(x = shk[["type"]], y = "uniform")) {
            cat(shk[["shock"]],
                file = write_path,
                sep = "\n",
                append = TRUE)
          } else if (identical(x = shk[["type"]], y = "custom")) {
            .write_custom_shk(
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
  } else if (is.element(el = write_object, set = c("closure", "tabfile"))) {
    writeLines(
      text = input,
      con = write_path
    )
  }
  names(x = write_path) <- write_object
  return(write_path)
}
