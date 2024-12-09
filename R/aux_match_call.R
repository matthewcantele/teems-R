#' Match Call Utility Function
#'
#' This internal utility function captures and reconstructs the function call
#' from its parent environment, ensuring that all formal arguments of the function
#' are included, even if they were not explicitly passed in the call. This is
#' particularly useful for debugging and logging function calls with their
#' complete set of arguments.
#'
#' @param ... Arguments to be captured.
#'
#' @return A call object that represents the reconstructed function call
#'         including default values for missing arguments.
#'
#' @keywords internal
#' @noRd
.match_call <- function(fun_call,
                        eval_args = NULL,
                        ...) {
  #browser()
  call <- evalq(expr = match.call(call = fun_call,
                                  expand.dots = FALSE,
                                  envir = environment()),
                envir = parent.frame(1L))

  formals <- evalq(expr = formals(), envir = parent.frame(1L))

  if (!is.null(x = eval_args)) {
    eval_args <- as.character(x = eval_args)
    for (arg in eval_args) {
      if (arg %in% names(call)) {
        call[[arg]] <- eval(call[[arg]], envir = parent.frame(1L))
      }
    }
  }

  for (i in setdiff(x = names(x = formals), names(x = call))) {
    call[i] <- list(formals[[i]])
  }

  match.call(sys.function(sys.parent()), call)
}
