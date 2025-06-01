#' @importFrom rlang current_env
#' 
#' @keywords internal
#' @noRd
.selective_eval <- function(call,
                            envir,
                            eval_args = NULL,
                            ...) {
  browser()
  call <- evalq(
    expr = match.call(
      call = call,
      expand.dots = FALSE,
      envir = rlang::current_env()
    ),
    envir = envir
  )

  formals <- evalq(expr = formals(), envir = envir)

  if (!is.null(x = eval_args)) {
    for (arg in eval_args) {
      eval_args <- as.character(x = eval_args)
      if (arg %in% names(call)) {
        call[[arg]] <- eval(call[[arg]], envir = envir)
      }
    }
  }

  for (i in setdiff(x = names(x = formals), names(x = call))) {
    call[i] <- list(formals[[i]])
  }

  revised_call <- match.call(sys.function(sys.parent()), call)
  final_call <- as.list(x = revised_call[-1])
  return(final_call)
}
