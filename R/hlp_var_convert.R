#' @importFrom purrr pluck
#' 
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
