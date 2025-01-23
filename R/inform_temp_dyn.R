#' @importFrom cli cli_inform
#'
#' @keywords internal
#' @noRd
.inform_temp_dyn <- function(intertemporal,
                             quiet,
                             call) {

  temporal_dynamics <- ifelse(test = intertemporal,
                              yes = "intertemporal",
                              no = "static")

  cli::cli_inform(
    c("i" = "Temporal dynamics have been determined as: {.val {temporal_dynamics}}",
      "i" = if (intertemporal) {paste("For intertemporal model run specifications, see {.fun teems::teems_time}")
    } else {
      NULL
    }),
    call = call
  )
}

