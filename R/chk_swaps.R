#' @keywords internal
#' @noRd
.check_swaps <- function(swap_in,
                         swap_out,
                         call) {
  if (!is.null(x = call[["swap_in"]])) {
    if (!is.symbol(x = call[["swap_in"]])) {
      call <- .harmonize_swap(raw_swap = swap_in, call = call)
    } else if (is.null(x = attr(x = swap_in, which = "full_swap"))) {
      .cli_action(action = "abort",
                  msg = "Invalid {.arg swap_in} value detected.",
                  call = call)
    }
  }
  if (!is.null(x = call[["swap_out"]])) {
    if (!is.symbol(x = call[["swap_out"]])) {
      call <- .harmonize_swap(raw_swap = swap_out, call = call)
    } else if (is.null(x = attr(x = swap_out, which = "full_swap"))) {
      .cli_action(action = "abort",
                  msg = "Invalid {.arg swap_out} value detected.",
                  call = call)
    }
  }
  return(call)
}