#' @importFrom purrr map_chr map2 pluck
#' 
#' @keywords internal
#' @noRd
.rename_headers <- function(input,
                            target_format,
                            coeff_extract) {
  names(input) <- purrr::map_chr(input, "header")

  origin_format <- ifelse(target_format %=% "v6.2", "v7.0", "v6.2")

  r_idx <- match(names(input), coeff_extract$header)

  input <- purrr::map2(
    .x = input,
    .y = r_idx,
    .f = function(header, id) {
      if (!is.na(id)) {
        header$coefficient <- purrr::pluck(coeff_extract[id, ], "coefficient", 1)
        header$label <- purrr::pluck(coeff_extract[id, ], "label", 1)
      } else {
        header$coefficient <- header$header
        header$label <- NA
      }

      if (is.numeric(header$data)) {
        nme_dimname <- names(dimnames(header$data))
        if (!is.null(nme_dimname)) {
          r_idx <- match(
            nme_dimname,
            with(
              set_conversion,
              get(paste0(origin_format, "name"))
            )
          )

          new_names <- with(
            set_conversion,
            get(paste0(target_format, "name"))[r_idx]
          )

          if (any(is.na(new_names))) {
            revert <- which(is.na(new_names))
            new_names[revert] <- nme_dimname[revert]
          }
          names(dimnames(header$data)) <- new_names
        }
      }
      return(header)
    }
  )
}