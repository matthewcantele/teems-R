#' @keywords internal
#' @noRd
.har_match <- function(con) {
  file_name <- basename(con)
  data_type <- switch(file_name,
                      "gddat.har" = "base",
                      "gsddat.har" = "base",
                      "gsdfdat.har" = "base",
                      "gdpar.har" = "par",
                      "gsdpar.har" = "par",
                      "gsdfpar.har" = "par",
                      "gdpextra.har" = "par",
                      "gdset.har" = "set",
                      "gsdset.har" = "set",
                      "gsdfset.har" = "set",
                      NA_character_)

  if (identical(x = data_type, y = NA_character_)) {
    stop(paste("Unsupported HAR file detected at:", con))
  }

  return(data_type)
}
