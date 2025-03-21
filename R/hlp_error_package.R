#' @keywords internal
#' @noRd
.package_error <- function(error_var = NULL,
                           error_fun,
                           call) {
# add backtrace
  if (!is.null(x = error_var)) {
    var_nchar <- nchar(x = deparse(expr = error_var),
                       type = "width")
    var_package <- deparse(expr = error_var, width.cutoff = var_nchar)
  }
  
  fun_nchar <- sum(nchar(x = deparse(expr = error_fun),
                         type = "width"))
  fun_package <- deparse(expr = error_fun, width.cutoff = fun_nchar)

  var2env <- deparse(substitute(list2env(variables, envir = rlang::current_env())))
  error_inputs <- paste(c(error_var, var2env, error_fun), collapse = ";")
  return(error_inputs)
}