.shk_cat <- function(data,
                     value_col,
                     write_path) {
  browser()
  cat(unlist(x = data[, ..value_col]),
      file = write_path,
      sep = " ",
      append = TRUE
  )

  # separator between header/data sets
  cat("\n", ";", "\n", "\n",
      file = write_path,
      sep = "",
      append = TRUE
  )
}
