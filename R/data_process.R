#' @importFrom purrr compact
#'
#' @keywords internal
#' @noRd
.process_data <- function(i_data,
                          set_mappings,
                          call) {
  metadata <- attr(i_data, "metadata")
  i_data <- lapply(i_data,
                   .custom_mod,
                   i_data = i_data)
  i_data <- .array2DT(i_data = i_data)
  i_data <- .weight_param(
    i_data = i_data,
    data_format = metadata$data_format
  )

  i_data <- lapply(i_data,
    .aggregate_data,
    sets = set_mappings,
    ndigits = .o_ndigits()
  )
  i_data <- purrr::compact(i_data)
  attr(i_data, "metadata") <- metadata
  attr(i_data, "call") <- call
  if ("time_steps" %in% names(attributes(set_mappings))) {
    attr(i_data, "time_steps") <- attr(set_mappings, "time_steps")
  }
  return(i_data)
}