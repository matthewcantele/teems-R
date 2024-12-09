#' Convert Variable Formats
#'
#' This function converts between structured data formats and concatenated
#' string representations of variables. It handles two main types of input:
#' structured data (e.g., lists or data frames) and concatenated data (strings
#' that need parsing and conversion). The function ensures that the output is
#' either a more readable structured format or a compact string format,
#' depending on the input provided.
#'
#' @param structured_data A list or data frame where each element or row
#'   corresponds to a variable set that needs to be converted into a string
#'   format.
#' @param concatenated_data A vector of strings representing concatenated
#'   variable information that needs to be parsed and converted into a
#'   structured format.
#' @param var_name Name of variable.
#'
#' @return Depending on the input, returns either a list of strings (from
#'   structured data) or a structured list or data frame (from concatenated
#'   data).
#' @keywords internal
#' @noRd
.convert_var <- function(structured_data = NULL,
                         concatenated_data = NULL,
                         var_name = NULL,
                         drop_quotes = FALSE) {

  if (!is.null(x = structured_data)) {

    # unnest if necessary
    if (!is.data.frame(x = structured_data)) {
      structured_data <- structured_data[[1]]
    }

    # null_set condition
    if (!identical(x = var_name, y = structured_data)) {
    converted_var <- paste0(var_name,
                            "(\"",
                            do.call(what = paste, c(structured_data, sep = "\",\"")),
                            "\")")
    } else {
      converted_var <- var_name
    }
  } else if (!is.null(x = concatenated_data)) {
    var_name <- purrr::pluck(.x = strsplit(x = concatenated_data, split = "\\("), 1, 1)

    if (grepl(pattern = "\\(|\"", x = concatenated_data)) {
      remainder <- purrr::pluck(.x = strsplit(x = concatenated_data, split = "\\("), 1, 2)
      csv_ss <- sub(pattern = ")", replacement = "", x = remainder)
      if (drop_quotes) {
      csv_ss <- gsub(pattern = "\"", replacement = "", x = csv_ss)
      }
      converted_var <- strsplit(x = csv_ss, split = ",")
    } else {
      converted_var <- NA
    }
    names(converted_var) <- var_name
  } else {
    stop("This function converts between 'structured_data' and 'concatenated_data'. At least one must be provided.")
  }

  return(converted_var)
}
