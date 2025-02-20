#' .append_tablo function
#'
#' This function appends write information to a Tablo file by writing out
#' coefficients and headers. In the case of recursive runs, it appends the
#' appropriate dedicated data and parameter write statements to the tablo.
#'
#' @param tab A list representing the tablo to be augmented.
#'
#' @importFrom purrr map2
#' @return The augmented tablo.
#' @keywords internal
#' @noRd
.append_tablo <- function(tab,
                          coeff_extract) {

  # sets excluded
  # io_files <- io_files[!grepl(pattern = "sets", x = io_files)]

  # recursive addition goes here
  # writes multiple headers to a single associate file type
  # if (write_out[["iodata"]]) {
  #   io_file_base <- sub(pattern = ".csv", replacement = "", x = io_files)
  #
  #   # use the names of io_files to pull appropriate coefficients
  #   tab_appendix <- unlist(x = purrr::map2(
  #     .x = names(x = io_files),
  #     .y = io_file_base,
  #     .f = function(io, file) {
  #       header_name <- unlist(x = subset(x = tablo[["coeff_s"]], subset = {file == io}, select = "header"))
  #       information <- unlist(x = subset(x = tablo[["coeff_s"]], subset = {file == io}, select = "information"))
  #       # remove "#"
  #       information <- trimws(x = gsub(pattern = "#", replacement = "", information))
  #       paste(
  #         header_name,
  #         paste("to file", file, "header"),
  #         paste0('"', header_name, '"'),
  #         "longname",
  #         paste0('"', information, '";')
  #       )
  #     }
  #   ))
  #
  #   # append to tab file
  #   tablo[["tab"]] <- capture.output(cat(tablo[["tab"]],
  #     tab_appendix,
  #     sep = "\n"
  #   ))
  # }


  # coefficient writeout
  tab_appendix <- paste(
    coeff_extract[["name"]],
    "to file",
    coeff_extract[["name"]],
    "header",
    paste0('"', coeff_extract[["name"]], '"'),
    "longname",
    paste0('"', trimws(x = gsub(pattern = "#", replacement = "", x = coeff_extract[["information"]])), '"', ";")
  )

  # here we append still due to potential for more write statements
  tab <- paste(c(tab,
    "\n#!< Write statements follow >!#\n",
    "Write",
    tab_appendix),
    collapse = "\n"
  )

  return(tab)
}
