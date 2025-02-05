#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.har_meta <- function(DREL,
                      DVER,
                      data_type) {
  metadata <- list()
  if (identical(x = length(x = DREL), y = 1L)) {
    string <- purrr::pluck(strsplit(DREL, "_"), 1, 1)
    if (identical(x = string, y = "R9.0A")) {
      metadata[["database_version"]] <- "v9A"
    } else {
      metadata[["database_version"]] <- string
    }
    metadata[["reference_year"]] <- as.numeric(sub(".*_(\\d{4}).*", "\\1", DREL))
  } else {
    metadata[["database_version"]] <- DREL[1]
    metadata[["reference_year"]] <- as.numeric(sub("[A-Za-z]", "", DREL[2]))
  }


  if (data_type == "dat") {
    metadata[["data_format"]] <- switch(as.character(x = DVER),
                                        "5" = "v6.2",
                                        "6" = "v7.0",
                                        NULL)
  }
  return(metadata)
}
