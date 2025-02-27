#' @keywords internal
#' @noRd
.harmonize_swap <- function(raw_swap,
                            call) {
  swap_ <- substitute(raw_swap)
  if (is.character(x = raw_swap)) {
    call[[as.character(x = swap_)]] <- teems_swap(var = raw_swap)
  } else {
    for (swap in seq_along(raw_swap)) {
      input <- raw_swap[[swap]]
      if (is.null(x = attr(x = input, which = "full_swap"))) {
        call[[as.character(x = swap_)]][[swap + 1]] <- teems_swap(var = input)
      } else {
        call[[as.character(x = swap_)]][[swap + 1]] <- input
      }
    }
  }
  return(call)
}
