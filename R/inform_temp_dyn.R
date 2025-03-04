#' @keywords internal
#' @noRd
.inform_temp_dyn <- function(tab_file,
                             quiet) {

  intertemporal <- any(grepl(pattern = "(intertemporal)", x = readLines(con = tab_file)))

  if (!quiet) {
    temporal_dynamics <- if (intertemporal) {"intertemporal"} else {"static/recursive"}
    .cli_action(action = "inform",
                msg = "Temporal dynamics have been determined as: {.val {temporal_dynamics}}"
    )
  }
  return(intertemporal)
}

