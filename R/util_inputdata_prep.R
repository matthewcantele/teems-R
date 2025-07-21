#' @importFrom purrr map2_chr
#' @importFrom rlang cnd_signal
#' @noRd
#' @keywords internal
.prep_input_data <- function(data,
                             data_class,
                             call) {
  withCallingHandlers(
    prepped_input <- purrr::map2_chr(
      data,
      names(data),
      function(d, nme) {
        if (!inherits(d, data_class)) {
          errant_class <- d
          .cli_action(load_err$nested_class,
            action = "abort",
            call = call
          )
        }
        if (inherits(d, "numeric")) {
          d <- data.frame(Value = d)
        }
        
        if (inherits(d, "character")) {
          path <- .check_input(
            file = d,
            valid_ext = "csv",
            call = call
          )
          df <- read.csv(path)
          if (!colnames(df)[ncol(df)] %=% "Value") {
            .cli_action(load_err$no_val_col,
              action = "abort",
              call = call
            )
          }
          return(path)
        } else if (inherits(d, "data.frame")) {
          if (!colnames(d)[ncol(d)] %=% "Value") {
            .cli_action(load_err$no_val_col,
              action = "abort",
              call = call
            )
          }

          path <- .teems_cache(
            input = d,
            file = nme,
            ext = "csv",
            dir = "csv_files"
          )
        }
        return(path)
      }
    ),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )
  return(prepped_input)
}