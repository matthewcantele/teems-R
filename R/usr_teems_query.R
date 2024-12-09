#' @importFrom purrr pluck
#' @export
teems_query <- function(component,
                        database = NULL,
                        model = NULL,
                        name = NULL) {
  if (identical(x = component, y = "tab")) {
    cat(con = internal_tab[[model]])
  } else if (identical(x = component, y = "closure")) {
    cat(con = closures[[model]], sep = "\n")
  } else if (identical(x = component, y = "mapping")) {
    if (is.null(x = database)) {
      stop("`Database` specification necessary to retrieve mappings.")
    } else if (is.null(x = name)) {
      mappings[[database]]
    } else if (is.null(names(x = name))){
      purrr::pluck(.x = mappings, database, name)
    } else {
      full <- colnames(x = mappings[[database]][[names(x = name)]])[1]
      col_match <- c(full, name)
      subset(x = mappings[[database]][[names(x = name)]],
             select = col_match)
    }
  } else if (identical(x = component, y = "var")) {
    if (is.null(x = model)) {
      stop(paste("Model specification necessary to extract variable information. Current models are:",
                 toString(names(x = var_extracts))))
    }

    if (is.null(x = name)) {
        var_extracts[[model]]
      } else {
        extract <- purrr::pluck(.x = var_extracts, model)
        variable_details <- extract[extract[["name"]] == name, c("name",
                                                                 "information",
                                                                 "qualifier_list",
                                                                 "full_set",
                                                                 "ls_mixed_idx",
                                                                 "ls_upper_idx")]

        colnames(variable_details) <- c("name",
                                        "information",
                                        "qualifiers",
                                        "tablo_entry",
                                        "mixed_format",
                                        "standard_format")

        .query_printout(data = variable_details)
      }
  }

}
