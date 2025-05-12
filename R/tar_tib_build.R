#' @importFrom tibble as_tibble
#' @importFrom data.table rbindlist setkey
#' 
#' @keywords internal
#' @noRd
.build_tibble <- function(ls_data,
                          preagg_header_replace,
                          comp_extract) {

  data_type <- attr(x = ls_data, which = "data_type")

  # header_replace if != NULL
  if (!is.null(x = preagg_header_replace)) {
    for (header in seq_along(preagg_header_replace)) {
      new_header <- preagg_header_replace[header]
      header_name <- names(x = new_header)

      if (!rlang::is_integerish(x = new_header)) {
        # check that file exists
        if (!file.exists(new_header)) {
          stop(paste(
            "File with header replacement data for header:",
            header_name,
            "not found."
          ))
        }

        # load file
        new_header_data <- data.table::fread(input = new_header)

        if (!is.element(el = "Value", set = colnames(x = new_header_data))) {
          stop(paste(
            dQuote(x = "Value"),
            "column missing from the pre-agg header:",
            header_name
          ))
        }

        # load data to be replaced
        old_header_data <- purrr::pluck(.x = ls_data, header_name, "dt")

        # check that datasets are equal
        if (!isTRUE(x = all.equal(
          current = old_header_data[, !"Value"],
          target = new_header_data[, !"Value"],
          ignore.row.order = TRUE,
          ignore.col.order = TRUE
        ))) {
          stop(paste(
            "One or more columns and/or rows in the new pre-agg header data for:",
            header_name,
            "is inconsistent or missing."
          ))
        }

        # replace data
        purrr::pluck(.x = ls_data, header_name, "dt") <- new_header_data
      } else {
        purrr::pluck(.x = ls_data, header_name, "dt", "Value") <- new_header
      }
    }
  }

  # create and write out metadata file
  metadata <- data.table::rbindlist(l = lapply(X = ls_data, FUN = function(header) {
    if (!identical(x = data_type, y = "set")) {
      list(
        header = header[["header_name"]],
        information = header[["information"]],
        coeff = header[["coefficient"]],
        type = header[["type"]],
        aggregate = header[["aggregate"]]
      )
    } else {
      list(
        header = header[["header_name"]],
        information = header[["information"]],
        type = header[["type"]],
        aggregate = header[["aggregate"]]
      )
    }
  }))

  # convert to tibble
  tib_data <- tibble::as_tibble(x = metadata)

  # add attributes
  attr(x = tib_data, which = "data_type") <- data_type

  # join metadata with data (match unnecessary but to be safe...)
  r_idx <- match(x = names(x = ls_data), table = tib_data[["header"]])

  # check metadata/data match
  if (any(is.na(x = r_idx))) {
    stop("NA found in match between metadata and data")
  }

  # add write to file here
  r_idx <- match(x = tib_data[["header"]], table = comp_extract[["header"]])
  tib_data[["input_file"]] <- comp_extract[["file"]][r_idx]

  tib_data[["dt"]] <- lapply(X = ls_data, FUN = function(d) {
    d[["dt"]]
  })

  # check for mismatches
  if (!all(is.element(
    el = names(x = tib_data[["dt"]]),
    set = tib_data[["header"]]
  ))) {
    stop("Name mismatch after metadata/data merge")
  }

  # names for later
  names(x = tib_data[["information"]]) <- tib_data[["header"]]
  names(x = tib_data[["type"]]) <- tib_data[["header"]]
  names(x = tib_data[["aggregate"]]) <- tib_data[["header"]]

  # sort data
  lapply(X = tib_data[["dt"]], FUN = data.table::setkey)

  return(tib_data)
}