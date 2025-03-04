#' @importFrom purrr map2
#' @importFrom data.table fread data.table setnames
#' @importFrom tibble as_tibble
#' 
#' @keywords internal
#' @noRd
.unite_csvs <- function(target,
                        paths,
                        call) {
  # Check if the directories exist
  if (!all(sapply(X = paths, FUN = dir.exists))) {
    stop("One or more paths do not exist.")
  }

  full_path <- paths[grepl(pattern = target, x = paths)]

  # rewrite using only outputs (setele.csv has set elements)
  target_files <- list.files(path = full_path, full.names = TRUE)

  # this function brings together and consolidates the various csvs belonging to each dir
  dat <- lapply(X = target_files, FUN = data.table::fread, header = FALSE, skip = 1, sep = ",")

  ls_inputs <- purrr::map2(
    .x = target_files,
    .y = dat,
    .f = function(f, d) {
      # concat subsetid file output
      if (ncol(x = d) > 2) {
        to_collapse <- d[, -1]
        # not clear why do.call needed here
        d <- data.table::data.table(
          V1 = d[["V1"]],
          V2 = do.call(what = paste, args = c(to_collapse, sep = ","))
        )
      }

      # indexing via number problematic
      nmes <- sub(
        pattern = ".csv",
        replacement = "",
        basename(path = f)
      )

      data.table::setnames(d,
        old = c("V1", "V2"),
        new = c("r_idx", nmes)
      )

      return(d)
    }
  )

  output <- Reduce(f = function(x, y) {
    merge(x, y, by = "r_idx")
  }, x = ls_inputs)

  output_tbl <- tibble::as_tibble(x = output)
  return(output_tbl)
}
