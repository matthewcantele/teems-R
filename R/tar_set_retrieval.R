#' Retrieve Model Sets with Mappings
#'
#' This function retrieves and processes model sets based on provided mappings,
#' ensuring that all required mappings are present and correctly formatted. It
#' handles inconsistencies in database versions and ensures the correct order of
#' sets according to the mappings.
#'
#' @param set_mappings A named list where each name corresponds to a set name
#'   and each value is the path to the mapping file for that set.
#' @param database_version The version of the database, used to handle
#'   inconsistencies across reference years.
#' @param nonint_sets A list of non-intertemporal sets, where each set contains
#'   a "header" and "dt" (data table).
#' @param model_sets A data frame or list of model sets to be retrieved,
#'   including their types, names, qualifier arguments, and indices.
#' @param mapping_files All possible mappings. Included here for tracking
#'   purposes.
#'
#' @importFrom purrr pmap
#' @importFrom data.table fread setnames
#' @return A list of data tables, each representing a model set with mappings
#'   applied. The function stops and throws an error if required mappings are
#'   missing or if mapping files do not have the correct format.
#' @keywords internal
#' @noRd
.retrieve_sets <- function(set_mappings,
                           database_version,
                           nonint_sets,
                           model_sets,
                           margin_sectors = c("atp", "otp", "wtp"),
                           data_format) {

  r_idx <- match(x = names(x = set_mappings), table = model_sets[["name"]])
  set_headers <- model_sets[["header"]][r_idx]

  r_idx <- match(x = set_headers, nonint_sets[["header"]])
  full_sets <- nonint_sets[["dt"]][r_idx]
  names(x = full_sets) <- names(x = set_mappings)

  # read in set mappings
  model_sets <- purrr::pmap(
    .l = list(full_sets, set_mappings, names(x = full_sets)),
    .f = function(set, map, set_name) {
      if (!is.null(x = map)) {
        if (grepl(pattern = "\\.csv", x = map)) {
          mapping <- data.table::fread(input = map)

          # check user-provided mapping
          # str check
          dim_check <- dim(set)
          dim_check[length(dim_check)] <- dim_check[length(dim_check)] + 1
          dim_check <- as.integer(x = dim_check)

          if (!identical(x = dim(x = mapping),
                         y = dim_check)) {
            stop(paste("The provided set mapping for",
                       set_name,
                       "does not conform to the expected dimensions:",
                       toString(x = dim_check)))
          }

          # ele check
          if (!all(is.element(el = unlist(x = mapping[,1]), set = unlist(set)))) {
            missing_ele <- unlist(x = mapping[,1])[!is.element(el = unlist(x = mapping[,1]), set = unlist(set))]
            stop(paste("The following elements:",
                       toString(x = missing_ele),
                       "are missing from the",
                       dQuote(x = set_name),
                       "first column set mapping."))
          }
          mapping_colnames <- c(colnames(set), "mapping")
          data.table::setnames(x = mapping, new = mapping_colnames)
          return(mapping)
        } else {
          internal_map <- colnames(x = purrr::pluck(.x = mappings, database_version, data_format, set_name))
          # pull mapping from internal data
          mapping <- subset(
            x = purrr::pluck(.x = mappings, database_version, data_format, set_name),
            select = c(internal_map[1], map)
          )

          data.table::setnames(x = mapping, old = map, new = "mapping")
          return(mapping)
        }
      }
    }
  )

  if (identical(x = data_format, y = "v6.2")) {
    # capital and margin goods for v6.2
    model_sets[["CGDS_COMM"]] <- data.table::data.table(H9 = "zCGDS",
                                                        mapping = "zCGDS")

    tradeables_col <- colnames(x = model_sets[["TRAD_COMM"]])[1]
    model_sets[["MARG_COMM"]] <- subset(x = model_sets[["TRAD_COMM"]],
                                        subset = {
                                          is.element(el = get(x = tradeables_col),
                                                     set = margin_sectors)
                                        })

    data.table::setnames(x = model_sets[["MARG_COMM"]],
                         new = c("MARG", "mapping"))
  } else if (identical(x = data_format, y = "v7.0")) {
    # margin goods for v7.0
    model_sets[["MARG"]] <- subset(x = model_sets[["COMM"]],
                                        subset = {
                                          is.element(el = COMM, set = margin_sectors)
                                        })

    data.table::setnames(x = model_sets[["MARG"]],
                         old = "COMM",
                         new = "MARG")
  }

  return(model_sets)
}
