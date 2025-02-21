#' @keywords internal
#' @noRd
.extract_metadata <- function(ls_array,
                              model_version) {
    metadata <- attr(x = ls_array, which = "metadata")
    names(x = metadata) <- gsub(pattern = "\\.", replacement = "_", names(x = metadata))

    metadata[["orig_database_version"]] <- metadata[["database_version"]]
    metadata[["database_version"]] <- gsub(pattern = "(\\d.*?)[A-Za-z]",
                                           replacement = "\\1",
                                           x = metadata[["database_version"]])

    metadata[["model_version"]] <- model_version

    if (!identical(x = metadata[["data_format"]],
                   y = metadata[["model_version"]])) {
      metadata[["conversion"]] <- TRUE
    } else {
      metadata[["conversion"]] <- FALSE
    }

    return(metadata)
}
