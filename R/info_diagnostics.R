#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.inform_diagnostics <- function(elapsed_time,
                                model_log,
                                call) {
  if (any(grepl(pattern = "Accurate", model_log))) {
    accuracy_output <- model_log[grep("Accurate", model_log)]

    all_digits <- as.numeric(trimws(purrr::map_chr(
      strsplit(accuracy_output, "digits|none"),
      function(x) {
        x[length(x)]
      }
    )))

    total_var <- sum(all_digits)
    a4digits <- sum(all_digits[1:3])

    accurate_4 <- a4digits / total_var
    accuracy <- sprintf("%.0f%%", accurate_4 * 100)
    a_threshold <- .o_accuracy_threshold()
    elapsed_time <- elapsed_time[[3]]

    if (elapsed_time < 60) {
      elapsed_time <- sprintf("%.2fs", elapsed_time)
    } else if (elapsed_time < 3600) {
      elapsed_time <- sprintf("%dm %02ds", floor(elapsed_time / 60), elapsed_time %% 60)
    } else {
      elapsed_time <- sprintf("%dh %02dm", floor(elapsed_time / 3600), floor((elapsed_time %% 3600) / 60))
    }

    .cli_action(solve_info$elapsed_time,
      action = "inform"
    )

    if (accurate_4 < a_threshold) {
      a_threshold <- sprintf("%.0f%%", a_threshold * 100)
      .cli_action(solve_wrn$accuracy,
        action = c("warn", "inform"),
        call = call
      )
    } else if (.o_verbose()) {
      .cli_action(solve_info$accuracy,
        action = "inform"
      )
    }
  }

  return(invisible(NULL))
}