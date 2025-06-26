#' @importFrom purrr pluck
#' @importFrom rlang is_integerish
#' @importFrom data.table fread setcolorder
#'
#' @keywords internal
#' @noRd
.finalize_data <- function(tib_data,
                           aggregated_input,
                           sets) {
browser()
  # check and swap in any user-provided headers
  if (!is.null(x = aggregated_input)) {
    for (header in names(x = aggregated_input)) {
      if (!rlang::is_integerish(x = aggregated_input)) {
        if (any(!sapply(X = aggregated_input, file.exists))) {
          stop("One or more files for user-provided headers does not exist.")
        }
        # Read the CSV file into a dt
        custom_header <- data.table::fread(input = aggregated_input[[header]])
        header <- names(x = custom_header)

        if (!is.element(el = "Value", set = colnames(x = custom_header))) {
          stop(paste(
            dQuote(x = "Value"),
            "column missing from the post-agg header:",
            header
          ))
        }

        # default values
        header_template <- purrr::pluck(.x = tib_data, "dt", header)
        template_col <- colnames(x = header_template)

        # check that datasets are equal
        if (!isTRUE(x = all.equal(
          current = custom_header[, !"Value"],
          target = header_template[, !"Value"],
          ignore.row.order = TRUE,
          ignore.col.order = TRUE
        ))) {
          stop(paste(
            "One or more columns and/or rows in the new post-agg header data for:",
            header,
            "is inconsistent or missing."
          ))
        }

        # rearrange if necessary
        if (!identical(x = template_col, y = colnames(x = custom_header))) {
          data.table::setcolorder(x = custom_header, neworder = template_col)
        }

        # swap in
        tib_data[["dt"]][[header]] <- custom_header
      } else {
        purrr::pluck(.x = tib_data, "dt", header, "Value") <- aggregated_input
      }
    }
  }

  tib_data[["lead"]] <- purrr::pmap(
    .l = list(
      tib_data[["ls_upper_idx"]],
      tib_data[["type"]],
      tib_data[["label"]],
      tib_data[["header"]]
    ),
    .f = function(upper, type, label, header) {
      if (!identical(x = upper, y = "null_set")) {
        set_dim <- with(
          data = sets[["mapped_ele"]],
          expr = {
            lengths(x = mget(x = upper))
          }
        )
        set_dim <- paste(set_dim, collapse = " ")
      } else {
        set_dim <- 1
      }
      lead <- paste(
        set_dim,
        type,
        "spreadsheet header",
        paste0('"', header, '"'),
        "longname",
        paste0('"', label, '";')
      )
    }
  )

  # algo for WriteData()
  tib_data[["idx"]] <- sapply(X = tib_data[["dt"]], FUN = .get_index)

  # sort headers
  tib_data <- tib_data[order(tib_data[["header"]]), ]
  return(tib_data)
}