#' @importFrom data.table data.table rbindlist
#' 
#' @noRd
#' @keywords internal
.load_mappings <- function(set_mappings,
                           metadata,
                           time_steps,
                           call) {
  set_mappings <- .check_set_mappings(
    set_mappings = set_mappings,
    metadata = metadata,
    call = call
  )

  if (metadata$data_format %=% "v6.2") {
    set_mappings$MARG_COMM <- subset(
      set_mappings$TRAD_COMM,
      set_mappings$TRAD_COMM[, 1][[1]] %in% .o_margin_sectors()
    )

    CGDS <- data.table::data.table("zcgds", "zcgds")
    set_mappings$PROD_COMM <- data.table::rbindlist(list(set_mappings$TRAD_COMM, CGDS), use.names = FALSE)
  } else if (metadata$data_format %=% "v7.0") {
    set_mappings$MARG <- subset(
      set_mappings$COMM,
      set_mappings$COMM[, 1][[1]] %in% .o_margin_sectors()
    )
  }

  if (!is.null(time_steps)) {
    time_steps <- .check_time_steps(
      t0 = metadata$reference_year,
      time_steps = time_steps,
      call = call
    )
    attr(set_mappings, "time_steps") <- time_steps
  }
  
  return(set_mappings)
}