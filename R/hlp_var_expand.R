#' @importFrom data.table CJ setcolorder
#' 
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
            "Plain variable names without additional label to be provided when
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
