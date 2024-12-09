#' @keywords internal
#' @noRd
.format_closure <- function(closure,
                            tab_readline = 15000) {
  # inject new lines (exogenous) if there was an initial attempt to simply
  # partial_exo down to sets or subsets but this is actually more complicated
  # than it may appear depending on the degree of granularity in swaps and
  # initial closure list. see archive version for leftover attempts

  final_closure <- capture.output(cat(
    "exogenous",
    closure,
    ";",
    "rest endogenous",
    ";", sep = "\n"
  ))

  # the TABREADLINE definition (ha_cge_global.h) limits lines to 20000 characters
  # inject new lines if closure passes this limit with 500 buffer
  # Note: reduction to sets from expanded closure resolves this issue
  tab_readline <- 15000

  if (sum(nchar(x = final_closure)) > tab_readline) {
    modified_vector <- character(0)
    current_length <- 0

    for (i in seq_along(final_closure)) {
      current_length <- current_length + nchar(final_closure[i])
      # Insert "here" into the vector at 500 character intervals
      if (current_length >= tab_readline) {
        modified_vector <- c(modified_vector, ";", "exogenous")
        current_length <- 0
      }
      # Append the current element from the input vector to the modified vector
      modified_vector <- c(modified_vector, final_closure[i])
    }
    final_closure <- modified_vector
  }

  return(final_closure)
}
