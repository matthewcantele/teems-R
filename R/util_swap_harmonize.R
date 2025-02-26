#' @keywords internal
#' @noRd
.harmonize_swap <- function(raw_swap,
                            fun_call) {
  swap_ <- substitute(raw_swap)
  if (is.character(x = raw_swap)) {
    fun_call[[as.character(x = swap_)]] <- teems_swap(var = raw_swap)
  } else {
    for (swap in seq_along(raw_swap)) {
      input <- raw_swap[[swap]]
      if (is.null(x = attr(x = input, which = "swap"))) {
        fun_call[[as.character(x = swap_)]][[swap + 1]] <- teems_swap(var = input)
      }
    }
  }
  return(fun_call)
}
