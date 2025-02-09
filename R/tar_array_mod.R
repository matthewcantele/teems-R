#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.modify_array <- function(ls_array,
                          database_version,
                          sets,
                          time_steps = NULL,
                          base_year = NULL,
                          set_array = NULL) {

  data_type <- attr(x = ls_array, "data_type")

  if (identical(x = data_type, y = "par")) {
    # change ETRE set from ENDWS_COMM to ENDW_COMM and add null values for mobile factors
    if (is.element(el = database_version, set = c("v9", "v10"))) {
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

      r_idx <- match(x = dimnames(x = ETRE_data)[[1]],
                     table = dimnames(x = ENDW)[[1]])

      ENDW[r_idx] <- ETRE_data
      ls_array[["ETRE"]][["data"]] <- ENDW
    }
  }
  
  if (!is.null(x = time_steps)) {
    ALLTIME <- purrr::pluck(.x = sets, "elements", "ALLTIME")
    n_time_steps <- length(x = time_steps) + 1
    if (identical(x = data_type, y = "dat")) {
      purrr::pluck(.x = ls_array, "NTSP", "data") <- matrix(data = n_time_steps)
      purrr::pluck(.x = ls_array, "IRAT", "data") <- array(data = rep(purrr::pluck(.x = ls_array, "IRAT", "data"),
                                                                      n_time_steps),
                                                           dimnames = list(ALLTIME = ALLTIME))
    } else  if (identical(x = data_type, y = "par")) {
      purrr::pluck(.x = ls_array, "AYRS", "data") <- array(data = c(0, cumsum(x = time_steps)),
                                                           dimnames = list(ALLTIME = ALLTIME))
    }
  }
  
  # CGDS to zcgds, lowercase
  ls_array <- lapply(X = ls_array,
                     FUN = function(header) {
                       if (!identical(x = data_type, y = "set")) {
                         dimnames(x = header[["data"]]) <- lapply(X = dimnames(x = header[["data"]]),
                                                                  FUN = function(ele) {
                                                                    ele <- gsub(pattern = "CGDS",
                                                                                replacement = "zcgds",
                                                                                x = ele)
                                                                    ele <- tolower(x = ele)
                                                                  })
                       } else {
                         header[["data"]] <- tolower(x = gsub(pattern = "CGDS",
                                                              replacement = "zcgds",
                                                              x = header[["data"]]))
                       }
                       return(header)
                     })

  attr(x = ls_array, "data_type") <- data_type
  return(ls_array)
}