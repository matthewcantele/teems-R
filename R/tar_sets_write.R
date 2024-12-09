#' Write Sets to File
#'
#' This function writes sets of data to a specified file within a given
#' directory. It formats the data with headers, informations, and the number of
#' strings and their maximum length before writing to the file.
#'
#' @param sets A list containing the final sets to be written, including headers, data
#'   tables (dt), and informations. Produced by \code{.expand_sets()}.
#' @param file_name The name of the file to which the sets will be written.
#' @param out_dir The directory where the file will be created.
#'
#' @importFrom purrr pluck
#' @return The path to the written file.
#' @keywords internal
#' @noRd
.write_sets <- function(sets,
                        file_name,
                        out_dir) {
  # write to
  path <- file.path(out_dir, file_name)

  # sets that are directly read into the model
  sets <- subset(x = sets, subset = !is.na(header))

  # rename by header rather than name
  names(sets[["elements"]]) <- sets[["header"]]
  names(sets[["information"]]) <- sets[["header"]]
  names(sets[["name"]]) <- sets[["header"]]

  # write sets
  lapply(X = sets[["header"]], FUN = function(h) {
    # header name
    name <- h

    # set elements
    ele <- purrr::pluck(.x = sets, "elements", h)

    # set description
    header_info <- trimws(x = gsub(pattern = "#",
                            replacement = "",
                            x = purrr::pluck(.x = sets, "information", h)))

    set_name <- toupper(x = purrr::pluck(.x = sets, "name", h))

    info <- paste("Set", set_name, header_info)

    lead <- paste(
      length(x = ele),
      "Strings Length",
      max(sapply(X = ele, FUN = nchar)),
      "Header",
      paste0('"', name, '"'),
      "LongName",
      paste0('"', info, '";')
    )

    # write the header lead with attributes
    cat(lead,
      file = path,
      sep = "\n",
      append = TRUE
    )

    # write elements
    cat(ele,
        file = path,
        sep = "\n",
        append = TRUE)

    cat("\n",
      file = path,
      sep = "",
      append = TRUE
    )
  })

  message(paste(nrow(x = sets), "sets written to", path))
  return(path)
}
