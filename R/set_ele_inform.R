#' @importFrom cli col_blue cli_inform
#'
#' @keywords internal
#' @noRd
.inform_set_ele <- function(set_ele,
                        set_name,
                        model_set) {
  element_text <- cli::col_blue(sort(x = paste(set_ele[[1]])))

  # Use cli_inform to display the message
  cli::cli_inform(c(
    "i" = "Set elements for: {set_name}s ({model_set}) determined as:",
    ">" = "{element_text}"
  ))
}
