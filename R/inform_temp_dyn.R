#' @importFrom cli cli_inform
#'
#' @keywords internal
#' @noRd
.inform_temp_dyn <- function(tab_file,
                             call,
                             quiet) {


  intertemporal <- any(grepl(pattern = "(intertemporal)", x = readLines(con = tab_file)))

  if (!quiet) {
    temporal_dynamics <- if (intertemporal) {"intertemporal"} else {"static"}
  cli::cli_inform(
    c("i" = "Temporal dynamics have been determined as: {.val {temporal_dynamics}}",
      "i" = if (intertemporal) {paste("For intertemporal model run specifications, see {.fun teems::teems_time}")
    } else {
      NULL
    }),
    call = call
  )
  }
  return(intertemporal)
}

