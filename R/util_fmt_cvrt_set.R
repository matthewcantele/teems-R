#' @importFrom purrr pluck map_chr
#'
#' @keywords internal
#' @noRd
.convert_format.set <- function(input,
                                set_extract,
                                target_format) {
  browser()
  if (target_format %=% "v7.0") {
    # change header names and duplicate H2/COMM for ACTS
    input <- lapply(
      input,
      function(header) {
        .convert_id(
          header = header,
          table = set_conversion,
          target_format = target_format
        )
      }
    )

    ACTS <- purrr::pluck(input, "H2")
    ACTS$header <- "ACTS"
    ACTS <- list(ACTS = ACTS)

    input <- c(input, ACTS)
  } else if (target_format %=%"v6.2") {
    # v7.0 to v6.2 on sets involves changing set names
    input <- lapply(
      input,
      function(header) {
        .convert_id(
          header = header,
          table = set_conversion,
          target_format = target_format
        )
      }
    )

    # add CGDS
    CGDS <- subset(
      set_conversion,
      v6.2header %in% "H9"
    )

    CGDS <- list(CGDS_COMM = list(
      header = CGDS$v6.2header,
      type = "character",
      label = CGDS$v6.2description,
      data = "CGDS",
      aggregate = FALSE
    ))

    input <- c(input, CGDS)
  }

  names(input) <- purrr::map_chr(input, "header")

  input <- purrr::map(input,
                      function(s) {
                        r_idx <- match(s$header, set_extract$header)
                        s$label <- purrr::pluck(set_extract, "label", r_idx)
                        return(s)
                      })
  
  keep <- with(set_conversion, get(paste0(target_format, "header")))
  input <- input[names(input) %in% keep]
  return(input)
}