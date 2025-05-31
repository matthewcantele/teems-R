#' @importFrom purrr pluck
#'
#' @keywords internal
#' @noRd
.convert_set_format <- function(set_array,
                                set_extract,
                                target_format) {
  if (identical(x = target_format, y = "v7.0")) {
    # change header names and duplicate H2/COMM for ACTS
    set_array <- lapply(
      X = set_array,
      FUN = function(header) {
        .convert_id(
          header = header,
          table = set_conversion,
          target_format = target_format
        )
      }
    )

    ACTS <- purrr::pluck(.x = set_array, "H2")
    ACTS[["header"]] <- "ACTS"
    ACTS <- list(ACTS = ACTS)

    set_array <- c(set_array, ACTS)
  } else if (identical(x = target_format, y = "v6.2")) {
    # v7.0 to v6.2 on sets involves changing set names
    set_array <- lapply(
      X = set_array,
      FUN = function(header) {
        .convert_id(
          header = header,
          table = set_conversion,
          target_format = target_format
        )
      }
    )

    # add CGDS
    CGDS <- subset(
      x = set_conversion,
      subset = {
        is.element(el = v6.2header, set = "H9")
      }
    )

    CGDS <- list(CGDS_COMM = list(
      header = CGDS[["v6.2header"]],
      type = "character",
      label = CGDS[["v6.2description"]],
      data = "CGDS",
      aggregate = FALSE
    ))

    set_array <- c(set_array, CGDS)
  }
  
  names(x = set_array) <- sapply(
    X = set_array,
    FUN = function(s) {
      s[["header"]]
    }
  )
  
  set_array <- sapply(X = set_array,
                      FUN = function(s) {
                        r_idx <- match(x = s[["header"]], table = set_extract[["header"]])
                        s[["label"]] <- set_extract[["label"]][r_idx]
                        return(s)
                      })
  return(set_array)
}