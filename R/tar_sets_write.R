#' @importFrom purrr pmap
#'
#' @keywords internal
#' @noRd
.write_sets <- function(sets,
                        out_dir) {
  # sets that are directly read into the model
  sets <- subset(x = sets, subset = !is.na(header))

  paths <- purrr::pmap(
    .l = list(
      sets[["header"]],
      sets[["name"]],
      sets[["mapped_ele"]],
      sets[["label"]],
      sets[["file"]]
    ),
    .f = function(header, set_name, ele, info, file) {
      # set description
      header_info <- trimws(x = gsub(
        pattern = "#",
        replacement = "",
        x = info
      ))

      set_name <- toupper(x = set_name)
      info <- paste("Set", set_name, header_info)
      lead <- paste(
        length(x = ele),
        "Strings Length",
        max(sapply(X = ele, FUN = nchar)),
        "Header",
        paste0('"', header, '"'),
        "LongName",
        paste0('"', info, '";')
      )

      path <- file.path(out_dir, paste0(file, ".txt"))
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
        append = TRUE
      )

      cat("\n",
        file = path,
        sep = "",
        append = TRUE
      )

      return(path)
    }
  )
  return(unique(x = unlist(x = paths)))
}
