#' @importFrom cli cli_abort
#'
#' @keywords internal
#' @noRd
.check_time_validity <- function(file,
                                 col,
                                 n_timestep,
                                 call) {
  input <- read.csv(file = file)
  arg <- as.character(substitute(expr = file))
  if (!all(is.element(el = colnames(x = input), set = col))) {
    cli::cli_abort(
      c("x" = "Columns within the user-provided file {.file {file}} for
        {.arg {arg}} do not conform to the expected: {.val {col}}."),
      call = call
    )
  } else {
    implied_time <- as.integer(x = sort(x = unique(x = input[["ALLTIMEt"]])))
    correct_time <- seq(from = 0, to = n_timestep - 1)
    if (!identical(x = implied_time, y = correct_time)) {
      cli::cli_abort(c("x" = "The user-provided ALLTIMEt set values for
                       {.arg {arg}} are not consistent with the implied
                       {.arg n_timestep} value of {.val {n_timestep}}."),
                     call = call)
    }
  }
return(file)
}
