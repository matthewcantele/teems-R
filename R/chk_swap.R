#' @keywords internal
.check_usr_swap <- function(raw_swap,
                            quiet,
                            call) {
  if (!is.null(x = raw_swap)) {
    if (!is.swap(x = raw_swap)) {
      # single character
      if (is.list(x = raw_swap)) {
        # swap in a list
        if (is.swap(x = raw_swap[[1]])) {
          processed_swap <- raw_swap[[1]]
        } else {
          .cli_action(
            msg = "Invalid list object supplied as swap.",
            action = "abort",
            call = call
          )
        }
      } else {
        processed_swap <- do.call(what = teems_swap, args = list(raw_swap))[[1]]
      }
    } else {
      # single swap
      processed_swap <- raw_swap
    }
  }
  return(processed_swap)
}