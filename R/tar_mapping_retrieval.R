#' @keywords internal
#' @noRd
.retrieve_mappings <- function(region_mapping,
                               sector_mapping,
                               endowment_mapping,
                               data_format) {
  # Model-specific requisite sets
  if (identical(x = data_format, y = "v6.2")) {
    # v6.2
    s_mappings <- list(
      REG = region_mapping,
      TRAD_COMM = sector_mapping,
      ENDW_COMM = endowment_mapping
    )
  } else if (identical(x = data_format, y = "v7.0")) {
    # v7
    s_mappings <- list(
      REG = region_mapping,
      COMM = sector_mapping,
      ACTS = sector_mapping,
      ENDW = endowment_mapping
    )
  }

  return(s_mappings)
}
