#' @importFrom purrr compact pluck map2
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
  ls_array <- purrr::compact(c(dat_array, par_array, aux_array))
  if (!is.null(full_exclude)) {
    ls_array <- ls_array[!names(ls_array) %in% full_exclude]
  }

  r_idx <- match(names(ls_array), coeff_extract$header)
  ls_array <- purrr::map2(
    ls_array,
    r_idx,
    function(h, id) {
      if (!is.na(id)) {
        coeff_name <- purrr::pluck(coeff_extract, "coefficient", id)
        label <- purrr::pluck(coeff_extract, "label", id)
        purrr::pluck(h, "coefficient") <- coeff_name
        purrr::pluck(h, "label") <- label
      } else {
        purrr::pluck(h, "coefficient") <- h$header
        purrr::pluck(h, "label") <- NA
      }
      return(h)
    }
  )

  # change ETRE set from ENDWS_COMM to ENDW_COMM and add null values
  if (metadata$database_version %in% c("v9", "v10")) {
    if (metadata$data_format %=% "v6.2") {
      ENDW_ele <- purrr::pluck(set_array, "H6", "data")
      nmes <- "ENDW_COMM"
      ETRE <- array(
        data = 0,
        dim = length(ENDW_ele),
        dimnames = list(ENDW_ele)
      )

      ETRE[["land"]] <- -1.000
      ETRE[["natlres"]] <- -0.001
    } else if (metadata$data_format %=% "v7.0") {
      ENDW_ele <- purrr::pluck(set_array, "ENDW", "data")
      REG_ele <- purrr::pluck(set_array, "REG", "data")
      nmes <- c("ENDW", "REG")
      ETRE <- array(
        0,
        c(length(ENDW_ele), length(REG_ele)),
        list(ENDW_ele, REG_ele)
      )
      ETRE[["land", ]] <- -1.000
      ETRE[["natlres", ]] <- -0.001
    }


    names(dimnames(ETRE)) <- nmes

    purrr::pluck(ls_array, "ETRE", "data") <- ETRE
  }

  # CGDS to zcgds, lowercase
  ls_array <- lapply(
    ls_array,
    FUN = function(header) {
      if (!is.null(dimnames(header$data))) {
        dimnames(header$data) <- lapply(
          dimnames(header$data),
          function(e) {
            e <- gsub("CGDS", "zcgds", e, ignore.case = TRUE)
            e <- tolower(e)
            return(e)
          }
        )
      }
      return(header)
    }
  )

  return(ls_array)
}