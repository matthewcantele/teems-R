#' Retrieve Mappings for Different Data Formats
#'
#' This function retrieves mappings for regions, sectors, margins, capital goods, and endowments
#' based on the specified data format version.
#'
#' @inheritParams .aggregate_data
#'
#' @param region_mapping A mapping of regions.
#' @param sector_mapping A mapping of sectors.
#' @param endowment_mapping A mapping of endowments.
#'
#' @return A list of mappings specific to the provided data format version.
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
