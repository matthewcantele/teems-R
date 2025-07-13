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
                         write_dir,
                         ...) {

  UseMethod(".TEEMS_write")
}

#' @export
.TEEMS_write.tabfile <- function(input,
                                 file,
                                 write_dir,
                                 prepend_file) {
  file <- basename(path = file)
  file <- paste(prepend_file, file, sep = "_")
  write_path <- file.path(write_dir, file)
  writeLines(
    text = input,
    con = write_path
  )

  names(write_path) <- class(input)
  return(write_path)
}

#' @export
.TEEMS_write.closure <- function(input,
                                 file,
                                 write_dir) {
  write_path <- file.path(write_dir, file)
  writeLines(
    text = input,
    con = write_path
  )
  names(write_path) <- class(input)
  return(write_path)
}

#' @export
.TEEMS_write.cmf <- function(input,
                             file,
                             write_dir) {
  write_path <- file.path(write_dir, file)
  cat(input,
    file = write_path,
    sep = "\n"
  )
  names(write_path) <- class(input)
  return(write_path)
}

#' @export
.TEEMS_write.shock <- function(input,
                               file,
                               write_dir,
                               ndigits) {
  write_path <- file.path(write_dir, file)

  if (!is.na(input)) {
    for (shk in input) {
      .write_shock(shock = shk,
                   write_path = write_path)
    }
  } else {
    cat("Shock ;",
        file = write_path,
        sep = "\n",
        append = FALSE)
  }
  names(write_path) <- class(input)
  return(write_path)
}

