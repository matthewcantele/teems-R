#' @importFrom purrr map2 pluck
#' 
#' @keywords internal
#' @noRd
.check_swap <- function(swap,
                        var_extract,
                        sets) {
  checked_swap <- purrr::map2(
    .x = swap,
    .y = names(x = swap),
    .f = function(components, var_name) {
      if (!identical(
        x = unlist(components, use.names = FALSE),
        y = NA
      )) {
        var_extract_set_names <- purrr::pluck(.x = var_extract, "ls_mixed_idx", var_name)

        # check that the names provided are correct
        if (!all(is.element(
          el = names(x = components),
          set = var_extract_set_names
        ))) {
          stop("One or more set names (LHS) in the 'swap_in' argument are
                             not found within the current set list")
        }

        # reorder and add uniform components if necessary
        if (!identical(x = names(x = components), y = var_extract_set_names)) {
          # omitted uniform components (ALLTIMEt = ALLTIMEt)
          missing_sets <- setdiff(x = var_extract_set_names, y = names(x = components))
          if (!identical(
            x = missing_sets,
            y = character(0)
          )) {
            ls_missing_sets <- as.list(x = missing_sets)
            names(x = ls_missing_sets) <- missing_sets
            components <- c(components, ls_missing_sets)
          }

          # ordering
          r_idx <- match(x = var_extract_set_names, table = names(x = components))
          components <- components[r_idx]
          set_names <- names(x = components)
        }

        # check that any individual elements provided exist in sets
        components <- purrr::map2(
          .x = components,
          .y = names(x = components),
          .f = function(ele, set) {
            standard_set <- substr(
              x = set,
              start = 1,
              stop = nchar(x = set) - 1
            )

            if (!identical(x = ele, y = set)) {
              set_elements <- with(
                data = sets[["elements"]],
                expr = get(x = standard_set)
              )

              if (any(!is.element(el = ele, set = set_elements))) {
                errant_ele <- ele[!is.element(el = ele, set = set_elements)]
                stop(paste(
                  "One or more individual elements specified in the closure:",
                  errant_ele,
                  "is(are) not found in the associated set:",
                  set
                ))
              }

              # add quotes for elements
              ele <- paste0("\"", ele, "\"")
              return(ele)
              # }
            } else {
              return(standard_set)
            }
          }
        )
        if (any(lapply(X = components, FUN = length) > 1)) {
          exp_components <- expand.grid(components, stringsAsFactors = FALSE)
          # apply the concatenation function to each combination
          concat_swap <- apply(
            X = exp_components,
            MARGIN = 1,
            FUN = function(row) {
              paste0(var_name, "(", paste(row, collapse = ","), ")")
            }
          )
        } else {
          concat_swap <- paste0(var_name, "(", paste(components, collapse = ","), ")")
        }
        return(concat_swap)
      } else {
        return(var_name)
      }
    }
  )

  return(checked_swap)
}