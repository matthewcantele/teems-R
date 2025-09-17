#' @importFrom data.table dcast.data.table
#'
#' @keywords internal
#' @noRd
.ems_check <- function(check,
                       outputs,
                       data,
                       model,
                       max_tolerance,
                       null_shock = FALSE) {
  call <- match.call()
  if (check %=% "baseline") {
    .check_baseline(outputs = outputs,
                    data = data,
                    model = model,
                    max_tolerance = max_tolerance,
                    null_shock = null_shock,
                    call = call)
  }
}