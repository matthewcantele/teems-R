#' @importFrom data.table set 
#' 
#' @keywords internal
#' @noRd
.map_data <- function(dt,
                      sets,
                      col) {

  col_mapping <- data.frame(
    pos = which(!colnames(dt) %in% c("Value", "sigma", "omega")),
    name = col,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(col_mapping))) {
    col_pos <- col_mapping$pos[i]
    set_col <- col_mapping$name[i]

    if (set_col %in% names(sets)) {
      table <- sets[[set_col]]
      r_idx <- match(dt[[col_pos]], tolower(table[, 1][[1]]))
      data.table::set(dt, j = col_pos, value = table[, 2][[1]][r_idx])
    }
  }
  return(dt)
}