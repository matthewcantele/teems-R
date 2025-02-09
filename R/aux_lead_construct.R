#' Construct Lead for Data Table
#'
#' This helper function constructs a lead string for a given header in a data
#' table. It determines the dimensionality of the data based on the sets
#' involved and the data type (integer for binary switches or real for other
#' types). The lead string includes the dimensionality, data type, and a
#' descriptive long name for the header.
#'
#' @param header_name Character. The header name for which the lead is being
#'   constructed.
#' @param dat A tibble containing data tables and their associated metadata.
#' @param sets A tibble containing the sets used in the model.
#'
#' @return A character string representing the lead for the specified header,
#'   including its dimensionality, data type, and a descriptive long name.
#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
.construct_lead <- function(header_name,
                            dat,
                            sets,
                            numeric_type = "Real") {
  # check that the header is present in 'dat'
  if (!is.element(
    el = header_name,
    set = names(x = dat[["dt"]])
  )) {
    stop(paste("Header:", header_name, "not found in 'dat'"))
  }

  # get specific header data
  header_data <- purrr::pluck(.x = dat, "dt", header_name)

  # if data colnames isn't "Value"
  if (!identical(x = colnames(x = header_data), y = "Value")) {
    # remove "Value"
    dt_colnames <- setdiff(
      x = colnames(x = header_data),
      y = "Value"
    )

    # drop final index (REGr to REG) for set interaction
    dt_colnames <- gsub(pattern = ".{1}$", replacement = "", x = dt_colnames)

    set_dim <- with(data = sets[["elements"]],
         expr = {
           sapply(X = mget(x = dt_colnames),
                  FUN = length)
         })

    set_dim <- paste(set_dim, collapse = " ")
  } else {
    set_dim <- 1
  }

  # data type (int for binary switches or real for the rest)
  if (identical(x = purrr::pluck(.x = dat, "type", header_name), y = "2IFULL")) {
    numeric_type <- "Integer"
  }

  # header lead
  lead <- paste(
    set_dim,
    numeric_type,
    "SpreadSheet Header",
    paste0('"', header_name, '"'),
    "LongName",
    paste0('"', purrr::pluck(.x = dat, "information", header_name), '";')
  )

  return(lead)
}
