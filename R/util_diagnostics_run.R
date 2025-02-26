#' @importFrom purrr map
#' 
#' @keywords internal
#' @noRd
.run_diagnostics <- function(out_file, error, singularity) {
  diagnostics_out <- ""
  accuracy_section <- ""

  if (any(grepl(pattern = "Accurate", x = out_file))) {
    accuracy <- out_file[grep(pattern = "Accurate", x = out_file)]

    # calculate % accurate to at least 4 digits
    totalVar <- sum(unlist(purrr::map(sapply(accuracy, strsplit, split = "digits|none"), function(s) {
      as.numeric(trimws(s[length(s)]))
    })))

    # accuracy through 4 digits
    a4digits <- sum(unlist(purrr::map(sapply(accuracy, strsplit, split = "digits|none"), function(s) {
      as.numeric(trimws(s[length(s)]))
    })[1:3]))

    accurateAt4 <- a4digits / totalVar
    if (accurateAt4 < 0.8) {
      warning_message <- paste(
        "Only", paste0(floor(accurateAt4 * 100), "%"),
        "of variables accurate at 4 digits, less than the recommended 80%."
      )
      # warning(warning_message)
      accuracy_section <- paste(accuracy_section, warning_message, sep = "\n")
    } else {
      success_message <- paste(paste0(floor(accurateAt4 * 100), "%"), "of variables accurate to 4 digits.")
      # message(success_message)
      accuracy_section <- paste(accuracy_section, success_message, sep = "\n")
    }

    # Adding accuracy levels to diagnostics_out
    accuracy_message <- paste(accuracy, collapse = "\n")
    accuracy_section <- paste(accuracy_section, accuracy_message, sep = "\n")
  }

  # Error and singularity checking
  if (error && singularity) {
    both_message <- "Both ERRORS and SINGULARITY DETECTED"
    # warning(both_message)
    diagnostics_out <- paste(diagnostics_out, both_message, sep = "\n")
  } else {
    if (error) {
      error_message <- "ERRORS DETECTED"
      # warning(error_message)
      diagnostics_out <- paste(diagnostics_out, error_message, sep = "\n")
    } else {
      no_error_message <- "No errors detected"
      # message(no_error_message)
      diagnostics_out <- paste(diagnostics_out, no_error_message, sep = "\n")
    }

    if (singularity) {
      singularity_message <- "SINGULARITY DETECTED"
      # warning(singularity_message)
      diagnostics_out <- paste(diagnostics_out, singularity_message, sep = "\n")
    } else {
      no_singularity_message <- "No singularity detected"
      # message(no_singularity_message)
      diagnostics_out <- paste(diagnostics_out, no_singularity_message, sep = "\n")
    }
  }

  # Combine accuracy section and diagnostics_out
  final_output <- paste(accuracy_section, diagnostics_out, sep = "\n")

  return(final_output)
}
