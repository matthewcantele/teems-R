#' @keywords internal
#' @noRd
.check_diagnostics <- function(out_file,
                               error,
                               singularity,
                               quiet) {
  if (any(grepl(pattern = "Accurate", x = out_file))) {
    accuracy_output <- out_file[grep(pattern = "Accurate", x = out_file)]

    # calculate % accurate to at least 4 digits
    totalVar <- sum(as.numeric(x = trimws(x = sapply(
      X = strsplit(
        x = accuracy_output,
        split = "digits|none"
      ),
      FUN = function(s) {
        s[length(x = s)]
      }
    ))))

    # accuracy through 4 digits
    a4digits <- sum(as.numeric(x = trimws(x = sapply(
      X = strsplit(
        x = accuracy_output,
        split = "digits|none"
      ),
      FUN = function(s) {
        s[length(x = s)]
      }
    )))[1:3])

    accurateAt4 <- a4digits / totalVar
    accuracy <- paste0(floor(accurateAt4 * 100), "%")
    if (accurateAt4 < 0.8) {
      .cli_action(
        action = "warn",
        msg = "Only {.emph {accuracy}} of variables accurate at at
                  least 4 digit precision, less than the recommended 80%."
      )
    } else {
      if (!quiet) {
        .cli_action(
          action = "inform",
          msg = "{.emph {accuracy}} of variables accurate at at least 4 digit 
          precision."
        )
      }
    }
  }

  # Error and singularity checking
  if (error && singularity) {
    .cli_action(
      action = "warn",
      msg = "Both errors and singularity detected."
    )
  } else {
    if (error) {
      .cli_action(
        action = "warn",
        msg = "Errors detected."
      )
    } else {
      if (!quiet) {
        .cli_action(
          action = "inform",
          msg = "No errors detected."
        )
      }
    }

    if (singularity) {
      .cli_action(
        action = "warn",
        msg = "Singularity detected."
      )
    } else {
      if (!quiet) {
        .cli_action(
          action = "inform",
          msg = "No singularity detected."
        )
      }
    }
  }
  return(invisible(NULL))
}
