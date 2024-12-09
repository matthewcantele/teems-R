#' Expand Variable Sets
#'
#' This function expands variable names into their respective sets based on
#' provided mappings and conditions. It handles both simple variable names and
#' sets of variables, applying specific transformations and checks to ensure the
#' correct expansion based on the input criteria.
#'
#' @param var_names A vector of variable names to be expanded; these should be
#'   simple variable names without additional qualifiers.
#' @param set_names A vector of set names which are to be expanded based on the
#'   elements they contain.
#' @param set_elements A list where each element corresponds to a set and
#'   contains the elements of that set.
#' @param var_extract A data frame or list that contains the mapping of
#'   variables to their respective sets.
#' @param sorted Logical; if TRUE, the resulting expansions are sorted,
#'   otherwise they are returned in the order they are found.
#'
#' @return A list of expanded variables or sets, depending on the input
#'   parameters. Each entry in the list corresponds to an expanded version of
#'   the input variable or set name.
#' @keywords internal
#' @noRd
.expand_var <- function(var_names = NULL,
                        set_names = NULL,
                        set_elements = NULL,
                        var_extract,
                        sorted = FALSE) {
  if (!is.null(x = var_names)) {
    expanded_var <- sapply(
      X = var_names,
      FUN = function(var) {
        # check if pure variable name
        if (!grepl(pattern = "\\(", x = var) &&
          !grepl(pattern = "\"", x = var)) {
          # get full var sets
          var_sets <- .get_sets(var = var,
                                var_extract = var_extract,
                                type = "upper")

          if (!identical(x = var_sets, y = "null_set")) {
            # "" to "null_set" for pfactwld et al in static
            expanded_var <- with(
              data = set_elements,
              expr = do.call(
                what = data.table::CJ,
                args = c(mget(x = var_sets, ifnotfound = ""), sorted = sorted)
              )
            )
          } else {
            expanded_var <- var
          }
        } else {
          stop(
            "Plain variable names without additional information to be provided when
                     using the 'var_names' argument."
          )
        }
      },
      simplify = FALSE
    )
  }

  if (!is.null(x = set_names)) {
    expanded_var <- sapply(
      X = set_names, FUN = function(var_components) {
        if (all(is.element(el = var_components, set = names(x = set_elements)))) {
          expanded_var <- with(
            data = set_elements,
            expr = do.call(
              what = data.table::CJ,
              args = c(mget(x = var_components), sorted = sorted)
            )
          )
        } else if (!all(is.element(el = var_components, set = names(x = set_elements)))) {
          ss_var <- var_components[is.element(el = var_components, set = names(x = set_elements))]
          ele_var <- var_components[!is.element(el = var_components, set = names(x = set_elements))]

          # remove quotations from singular elements
          ele_var <- gsub(pattern = "\"", replacement = "", x = ele_var)

          expanded_var <- with(
            data = set_elements,
            expr = do.call(
              what = data.table::CJ,
              args = c(ele_var, mget(x = ss_var), sorted = sorted)
            )
          )

          # ensure column order is retained
          colnames(expanded_var) <- sapply(
            X = colnames(x = expanded_var),
            FUN = function(col_name) {
              if (!is.element(
                el = col_name,
                set = var_components
              )) {
                col_name <- paste0("\"", unlist(x = unique(x = subset(x = expanded_var, select = col_name))), "\"")
              }
              return(col_name)
            }
          )

          # match colnames with initial var order and reorder if necessary
          r_idx <- match(x = var_components, table = colnames(expanded_var))
          data.table::setcolorder(x = expanded_var, neworder = r_idx)
          return(expanded_var)
        }
      },
      simplify = FALSE
    )
  }

  return(expanded_var)
}
