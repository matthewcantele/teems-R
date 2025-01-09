#' Check and Update Mapping for Model Sets
#'
#' This function checks the mapping of margin commodities and tradables within
#' model sets against a full list of sets. It ensures that margin commodities
#' are a subset of tradables and updates the mapping accordingly. Additionally,
#' it verifies that all elements within the original sets have been provided a
#' mapping and compiles the final values for each set.
#'
#' @param model_sets A list containing the model sets with their mappings.
#' @param full_sets A data frame or list containing the full list of sets for
#'   comparison and updating.
#'
#' @importFrom data.table data.table
#' @importFrom purrr pluck pmap
#' @return A list containing the updated sets with their mappings, including the
#'   final values for each set.
#' @keywords internal
#' @noRd
.check_mapping <- function(model_sets,
                         full_sets) {

  # first identifying header column for each set
  c1 <- sapply(X = model_sets, FUN = function(c) {
    colnames(x = c)[1]
  })

  # keep only necessary sets
  sets <- subset(x = full_sets, subset = is.element(el = header, set = c1))
  sets <- sets[match(x = c1, table = sets[["header"]]), ]

  # update set_tib with mapping
  r_idx <- match(x = c1, table = sets[["header"]])
  sets[["full_sets"]] <- model_sets[r_idx]

  # check that mappings include all associated elements
  purrr::pmap(
    .l = list(
      sets[["dt"]],
      sets[["full_sets"]],
      names(x = sets[["full_sets"]])
    ),
    .f = function(orig_set, mapping, set_nme) {
      col <- colnames(x = orig_set)
      if (!all(is.element(el = unlist(x = orig_set), set = mapping[[col]]))) {
        stop(paste(
          "One or more elements:",
          unlist(x = orig_set)[!is.element(el = unlist(x = orig_set), set = mapping[[col]])],
          "belonging to set:", set_nme,
          "has not been provided a mapping"
        ))
      }
    }
  )

  # dt to hold final values (those written out)
  sets[["dt"]] <- lapply(X = sets[["full_sets"]], FUN = function(s) {
    data.table::data.table(Value = unique(x = s[["mapping"]]))
  })

  # actual set names
  sets[["set_name"]] <- names(x = sets[["dt"]])
  names(x = sets[["dt"]]) <- sets[["header"]]

  return(sets)
}
