#' @importFrom purrr pluck map2
#' 
#' @keywords internal
#' @noRd
.modify_array <- function(dat_array,
                          par_array,
                          aux_array,
                          set_array,
                          coeff_extract,
                          full_exclude,
                          metadata,
                          call) {

  ls_array <- purrr::compact(.x = c(dat_array, par_array, aux_array))
  # full exclude
  if (!is.null(x = full_exclude)) {
    ls_array <- ls_array[!is.element(el = names(x = ls_array), set = full_exclude)]
  }
  
  r_idx <- match(x = names(x = ls_array), table = coeff_extract[["header"]])
  ls_array <- purrr::map2(
    .x = ls_array,
    .y = r_idx,
    .f = function(h, id) {
      if (!is.na(x = id)) {
        coeff_name <- purrr::pluck(.x = coeff_extract, "coefficient", id)
        label <- purrr::pluck(.x = coeff_extract, "label", id)
        purrr::pluck(.x = h, "coefficient") <- coeff_name
        purrr::pluck(.x = h, "label") <- label
      } else {
        purrr::pluck(.x = h, "coefficient") <- h[["header"]]
        purrr::pluck(.x = h, "label") <- NA
      }
      return(h)
    }
  )

  # change ETRE set from ENDWS_COMM to ENDW_COMM and add null values for mobile factors
  if (is.element(el = metadata[["database_version"]], set = c("v9", "v10"))) {
        if (identical(x = metadata[["data_format"]], y = "v6.2")) {
         ENDW_ele <- purrr::pluck(.x = set_array, "H6", "data")
         REG_ele <- purrr::pluck(.x = set_array, "H2", "data")
         nmes <- c("ENDW_COMM", "REG")
        } else if (identical(x = metadata[["data_format"]], y = "v7.0")) {
          ENDW_ele <- purrr::pluck(.x = set_array, "ENDW", "data")
          REG_ele <- purrr::pluck(.x = set_array, "REG", "data")
          nmes <- c("ENDW", "REG")
        }
    
     ETRE <- array(data = 0,
                   dim = c(length(ENDW_ele), length(REG_ele)),
                   dimnames = list(ENDW_ele, REG_ele))
    
     ETRE["land",] <- -1.000
     ETRE["natlres",] <- -0.001
     names(x = dimnames(x = ETRE)) <- nmes

    purrr::pluck(.x = ls_array, "ETRE", "data") <- ETRE
  }

  # CGDS to zcgds, lowercase
  ls_array <- lapply(
    X = ls_array,
    FUN = function(header) {
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
      return(header)
    }
  )

  return(ls_array)
}