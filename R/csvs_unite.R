#' @importFrom purrr map2
#' @importFrom data.table fread data.table setnames
#' @importFrom tibble as_tibble
#' 
#' @keywords internal
#' @noRd
.unite_csvs <- function(target,
                        paths,
                        call) {
  target_files <- list.files(grep(target, paths, value = TRUE),
    full.names = TRUE
  )
  output_data <- lapply(target_files,
    data.table::fread,
    header = FALSE,
    skip = 1,
    sep = ","
  )

  output_nmes <- sub("\\.csv", "", basename(target_files))

  output_data <- purrr::map2(
    output_data,
    output_nmes,
    function(dt, nme) {
      if (ncol(dt) > 2) {
        dt <- data.table::data.table(dt[, 1],
                                     do.call(paste, c(dt[, -1], sep = ",")))
      }
      data.table::setnames(dt, new = c("r_idx", nme))
      return(dt)
    }
  )

  output <- Reduce(function(x, y) {
    merge(x, y, by = "r_idx")
  }, output_data)

  output_tbl <- tibble::as_tibble(output)
  return(output_tbl)
}
