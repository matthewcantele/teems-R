#' @importFrom purrr pluck
.har_meta <- function(DREL,
                      DVER,
                      data_type) {
  metadata <- list()
  if (identical(x = length(x = DREL), y = as.integer(1))) {
    string <- purrr::pluck(strsplit(DREL, "_"), 1, 1)
    if (identical(x = string, y = "R9.0A")) {
      metadata[["database.version"]] <- "v9A"
    } else {
      metadata[["database.version"]] <- string
    }
    metadata[["reference.year"]] <- as.numeric(sub(".*_(\\d{4}).*", "\\1", DREL))
  } else {
    metadata[["database.version"]] <- DREL[1]
    metadata[["reference.year"]] <- as.numeric(sub("[A-Za-z]", "", DREL[2]))
  }


  if (data_type == "dat") {
    metadata[["data.format"]] <- switch(as.character(x = DVER),
                                        "5" = "v6.2",
                                        "6" = "v7.0",
                                        NULL)
  }
  return(metadata)
}
