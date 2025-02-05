#' @importFrom rlang current_env
#' @importFrom tibble tibble
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.update_set_names <- function(ls_array,
                              coeff_extract,
                              metadata) {
  # add our package type specific set names
  # new set names come from the coeff_extract if there is no conversion
  # and the coefficients are present
  # if conversion or not present generics are used
  # if conversion, the post-conversion names are taken from the abstract
  data_type <- attr(x = ls_array, "data_type")
  list2env(x = metadata, envir = rlang::current_env())
  set_table <- tibble::tibble(v6.2_upper = c("TRAD_COMM", "PROD_COMM", "ENDW_COMM", "MARG_COMM", "ALLTIME"),
                              v6.2_mixed = c("TRAD_COMMi", "PROD_COMMj", "ENDW_COMMi", "MARG_COMMm", "ALLTIMEt"),
                              v7.0_upper = c("COMM", "ACTS", "ENDW", "MARG", "ALLTIME"),
                              v7.0_mixed = c("COMMc", "ACTSs", "ENDWe", "MARGm", "ALLTIMEt"))
  if (!convert) {
    r_idx <- match(x = names(x = ls_array), table = coeff_extract[["header"]])
    new_names <- purrr::pluck(subset(x = coeff_extract,
                                     subset = {is.element(el = header,
                                                          set = names(ls_array))},
                                     select = ls_mixed_idx), 1)

    ls_array <- lapply(
      X = ls_array,
      FUN = function(h) {
        coeff <- h[["coefficient"]]
        new_names <- purrr::pluck(coeff_extract, "ls_mixed_idx", coeff)
        if (!identical(x = new_names, y = "ALLTIMEt")) {
        new_names <- setdiff(x = new_names, y = "ALLTIMEt")
        }
        old_names <- names(x = dimnames(x = h[["data"]]))
        # NULL if not in extract
        if (!is.null(x = new_names)) {
          if (!identical(x = new_names, y = "null_set")) {
            names(x = dimnames(x = h[["data"]])) <- new_names
          }
        } else if (!is.null(x = old_names)) {
          # NULL if null_set
          # multiple region condition
          if (identical(x = sum(grepl(pattern = "REG", x = old_names),
                                na.rm = TRUE),
                        y = 2L)) {
            old_names[duplicated(x = old_names)] <- "REGs"
            new_names <- sub(pattern = "^REG$",
                             replacement = "REGr",
                             x = old_names)
          } else if (identical(x = sum(grepl(pattern = "REG",
                                             x = old_names),
                                       na.rm = TRUE),
                               y = 1L)) {
            new_names <- sub(pattern = "^REG$",
                             replacement = "REGr",
                             x = old_names)

          }
          
          origin_col <- paste(data_format, "upper", sep = "_")
          dest_col <- paste(data_format, "mixed", sep = "_")
          vect_mapping <- setNames(set_table[[dest_col]], set_table[[origin_col]])
          
          new_names <- ifelse(test = is.element(el = old_names, set = names(x = vect_mapping)), 
                              vect_mapping[old_names], 
                              new_names)
          names(x = dimnames(x = h[["data"]])) <- new_names
        }
        return(h)
      }
    )
  } else {
    browser()
  }
  attr(x = ls_array, which = "data_type") <- data_type
  return(ls_array)
}