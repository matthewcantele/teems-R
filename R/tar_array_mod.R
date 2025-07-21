#' @importFrom purrr compact pluck map2
#' 
#' @keywords internal
#' @noRd
.modify_array <- function(dat_array,
                          par_array,
                          aux_array,
                          coeff_extract,
                          metadata,
                          call) {
  ls_array <- purrr::compact(c(dat_array, par_array, aux_array))
  param_weight_headers <- unique(gsub("-", "", unlist(param_weights[metadata$data_format])))
  ls_array <- ls_array[names(ls_array) %in% c(coeff_extract$header, param_weight_headers)]
  
  # drop_headers <- .o_full_exclude()
  # if (!is.null(.o_full_exclude())) {
  #   ls_array <- ls_array[!names(ls_array) %in% .o_full_exclude()]
  # }

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
  
  ls_array <- .update_set_names(
    ls_array = ls_array,
    coeff_extract = coeff_extract,
    data_format = metadata$data_format
  )

  return(ls_array)
}