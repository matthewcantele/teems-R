#' @importFrom purrr pluck map2
#' 
#' @keywords internal
#' @noRd
.modify_array <- function(ls_array,
                          sets,
                          coeff_extract,
                          header_rename,
                          coefficient_rename,
                          full_exclude,
                          metadata,
                          data_type,
                          time_steps = NULL,
                          set_array = NULL,
                          append = NULL,
                          call) {
  # records missing for the following parameter headers
  if (identical(x = data_type, y = "par")) {
    if (is.element(el = "RDLT", set = names(x = ls_array))) {
      purrr::pluck(ls_array, "RDLT", "coefficient") <- "RORDELTA"
    }
    if (is.element(el = "SLUG", set = names(x = ls_array))) {
      purrr::pluck(ls_array, "SLUG", "coefficient") <- "SLUG"
    }
  }

  # renaming headers if header_rename != NULL
  if (!is.null(x = header_rename)) {
    if (!all(is.element(el = names(x = header_rename), set = names(x = ls_array)))) {
      errant_headers <- names(x = header_rename)[!is.element(el = names(x = header_rename), set = names(x = ls_array))]
      n_errant_headers <- length(x = errant_headers)

      error_fun <- substitute(.cli_action(
        action = "abort",
        msg = "The HAR file provided: {.val {full_har_path}} does not contain
        {n_errant_headers} header{?s} specified for renaming,
        {.val {errant_headers}}.",
        call = call
      ))

      error_var <- substitute(variables <- list(
        errant_headers = errant_headers,
        n_errant_headers = n_errant_headers,
        full_har_path = full_har_path
      ))

      error_inputs <- .package_error(
        error_var = error_var,
        error_fun = error_fun,
        call = call
      )
      stop(message = error_inputs)
    }

    r_idx <- match(x = names(x = header_rename), table = names(x = ls_array))
    names(x = ls_array)[r_idx] <- header_rename

    for (nme in seq_along(header_rename)) {
      h <- header_rename[[nme]]
      purrr::pluck(.x = ls_array, h, "header_name") <- h
    }
  }

  # overwrite any data coefficient values with tablo coefficient value
  # the coefficient extraction code above is either off for v11 or some
  # coefficient values (MAKB) are incorrect

  r_idx <- match(x = names(x = ls_array), table = coeff_extract[["header"]])

  if (!identical(x = data_type, y = "set")) {
    ls_array <- purrr::map2(
      .x = ls_array,
      .y = r_idx,
      .f = function(h, id) {
        if (!is.na(x = id)) {
          coeff_name <- purrr::pluck(.x = coeff_extract, "name", id)
          purrr::pluck(.x = h, "coefficient") <- coeff_name
        }
        return(h)
      }
    )
  }

  # renaming coefficients if coefficient_rename != NULL
  if (!is.null(x = coefficient_rename)) {
    coefficient_names <- lapply(ls_array, function(h) {
      h[["coefficient"]]
    })
    if (!all(is.element(el = names(x = coefficient_rename), set = coefficient_names))) {
      errant_coefficients <- names(x = coefficient_rename)[!is.element(
        el = names(x = coefficient_rename),
        set = coefficient_names
      )]

      error_fun <- substitute(.cli_action(
        action = "abort",
        msg = "The coefficients(s) specified for renaming,
                  {.val {errant_coefficients}} is(are) not found in the
                  {.val {full_har_path}} HAR file.",
        call = call
      ))

      error_var <- substitute(variables <- list(
        errant_coefficients = errant_coefficients,
        full_har_path = full_har_path
      ))

      error_inputs <- .package_error(
        error_var = error_var,
        error_fun = error_fun,
        call = call
      )
      stop(message = error_inputs)
    }

    for (nme in seq_along(coefficient_rename)) {
      old_coeff <- names(x = coefficient_rename[nme])
      new_coeff <- coefficient_rename[[nme]]
      r_idx <- match(x = old_coeff, table = coefficient_names)
      purrr::pluck(.x = ls_array, r_idx, "coefficient") <- new_coeff
    }
  }

  # drop records and unnecessary data
  ls_array <- lapply(
    X = ls_array,
    FUN = function(h) {
      h <- h[!is.element(el = names(x = h), set = c("start", "binary", "records"))]
      return(h)
    }
  )

  if (!is.null(x = append)) {
    ls_array <- c(ls_array, append)
  }

  # set type and aggregate
  ls_array <- lapply(
    X = ls_array,
    FUN = function(h) {
      if (is.null(x = h[["aggregate"]])) {
        if (is.element(el = h[["type"]], set = c("1CFULL", "2IFULL", "RLFULL"))) {
          h[["aggregate"]] <- FALSE
        } else {
          h[["aggregate"]] <- TRUE
        }
        
        h[["type"]] <- switch(EXPR = h[["type"]],
                              "1CFULL" = "character",
                              "2IFULL" = "integer",
                              "RLFULL" = "character",
                              "REFULL" = "real",
                              "RESPSE" = "real")
      }
      return(h)
    }
  )
  
  # no need to adopt gtap obscure nomenclature here
  
  # 1CFULL = character
  # 2IFULL = integer
  # REFULL = real

  # full exclude
  if (!is.null(x = full_exclude)) {
    ls_array <- ls_array[!is.element(el = names(x = ls_array), set = full_exclude)]
  }

  if (identical(x = data_type, y = "par")) {
    # change ETRE set from ENDWS_COMM to ENDW_COMM and add null values for mobile factors
    if (is.element(el = metadata[["database_version"]], set = c("v9", "v10"))) {
      ETRE_data <- purrr::pluck(.x = ls_array, "ETRE", "data")
      ENDWS_dimnames <- unlist(x = dimnames(x = ETRE_data))

      # New elements to be added
      new_elements <- setdiff(
        x = purrr::pluck(.x = set_array, "H6", "data"),
        y = ENDWS_dimnames
      )

      # Combine existing and new elements
      ENDW_elements <- c(ENDWS_dimnames, new_elements)

      # Create a new array with the new dimensions and set all values to 0
      ENDW <- array(0, dim = length(ENDW_elements))
      dimnames(ENDW) <- list(ENDW_COMM = ENDW_elements)

      r_idx <- match(
        x = dimnames(x = ETRE_data)[[1]],
        table = dimnames(x = ENDW)[[1]]
      )

      ENDW[r_idx] <- ETRE_data
      ls_array[["ETRE"]][["data"]] <- ENDW
    }
  }

  if (metadata[["convert"]]) {
    ls_array <- .convert_data(
      ls_array = ls_array,
      data_format = metadata[["data_format"]],
      data_type = data_type,
      coeff_extract = coeff_extract
    )
  }

  # CGDS to zcgds, lowercase
  ls_array <- lapply(
    X = ls_array,
    FUN = function(header) {
      if (!identical(x = data_type, y = "set")) {
        if (!is.null(x = dimnames(x = header[["data"]]))) {
          dimnames(x = header[["data"]]) <- lapply(
            X = dimnames(x = header[["data"]]),
            FUN = function(e) {
              e <- gsub(
                pattern = "CGDS",
                replacement = "zcgds",
                x = e,
                ignore.case = TRUE
              )
              e <- tolower(x = e)
              return(e)
            }
          )
        }
      } else {
        header[["data"]] <- tolower(x = gsub(
          pattern = "CGDS",
          replacement = "zcgds",
          x = header[["data"]],
          ignore.case = TRUE
        ))
      }
      return(header)
    }
  )

  if (!is.null(x = time_steps)) {
    ALLTIME <- purrr::pluck(.x = sets, "elements", "ALLTIME")
    n_time_steps <- length(x = time_steps) + 1
    if (identical(x = data_type, y = "base")) {
      purrr::pluck(.x = ls_array, "NTSP", "data") <- matrix(data = n_time_steps)
      purrr::pluck(.x = ls_array, "IRAT", "data") <- array(
        data = rep(
          purrr::pluck(.x = ls_array, "IRAT", "data"),
          n_time_steps
        ),
        dimnames = list(ALLTIME = ALLTIME)
      )
    } else if (identical(x = data_type, y = "par")) {
      purrr::pluck(.x = ls_array, "AYRS", "data") <- array(
        data = c(0, cumsum(x = time_steps)),
        dimnames = list(ALLTIME = ALLTIME)
      )
    }
  }

  return(ls_array)
}