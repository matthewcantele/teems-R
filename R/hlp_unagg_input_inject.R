#' @importFrom data.table fread
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.inject_unagg_input <- function(ls_data,
                                unaggregated_input) {
  for (header in seq_along(unaggregated_input)) {
    new_header <- unaggregated_input[header]
    header <- names(x = new_header)

    # check that file exists
    if (!file.exists(new_header)) {
      stop(paste(
        "File with header replacement data for header:",
        header,
        "not found."
      ))
    }

    # load file
    new_header_data <- data.table::fread(input = new_header)

    if (!is.element(el = "Value", set = colnames(x = new_header_data))) {
      stop(paste(
        dQuote(x = "Value"),
        "column missing from the pre-agg header:",
        header
      ))
    }

    # load data to be replaced
    old_header_data <- purrr::pluck(.x = ls_data, header, "dt")

    # check that datasets are equal
    if (!isTRUE(x = all.equal(
      current = old_header_data[, !"Value"],
      target = new_header_data[, !"Value"],
      ignore.row.order = TRUE,
      ignore.col.order = TRUE
    ))) {
      stop(paste(
        "One or more columns and/or rows in the new pre-agg header data for:",
        header,
        "is inconsistent or missing."
      ))
    }

    # replace data
    purrr::pluck(.x = ls_data, header, "dt") <- new_header_data
  }
}