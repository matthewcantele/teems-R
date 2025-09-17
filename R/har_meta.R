#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.har_meta <- function(DREL,
                      DVER,
                      data_type,
                      call) {

  metadata <- list()
  if (length(DREL) %=% 1L) {
    string <- purrr::pluck(strsplit(DREL, "_"), 1, 1)
    if (string %=% "R9.0A") {
      metadata$database_version <- "v9A"
    } else {
      metadata$database_version <- string
    }
    metadata$reference_year <- as.numeric(sub(".*_(\\d{4}).*", "\\1", DREL))
  } else {
    metadata$database_version <- DREL[1]
    metadata$reference_year <- as.numeric(sub("[A-Za-z]", "", DREL[2]))
  }


  if (data_type %=% "dat") {
    if (is.null(DVER)) {
      .cli_action(data$err$wrong_input,
        action = "abort",
        call = call
      )
    }
    metadata$data_format <- switch(as.character(DVER),
      "5" = "v6.2",
      "6" = "v7.0",
      NULL
    )
  }
  return(metadata)
}