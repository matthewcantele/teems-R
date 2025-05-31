#' @keywords internal
#' @noRd
.shock_prep <- function(shocks,
                        var_extract) {
  

  # length 0 will be returned for at least one element on an unwrapped list
  if (any(is.element(el = 0, set = sapply(X = shocks, FUN = length)))) {
    shocks <- list(shocks)
  }

  # reconstruct shocks
  shock_list <- lapply(
    X = shocks,
    FUN = function(shock_arg) {
      shock_names <- shock_arg[length(shock_arg)][[1]]
      shock_arg <- head(x = shock_arg, -1)
      names(x = shock_arg) <- shock_names
      optional_sets <- c("shock_ele", "shock_set")
      if (any(is.element(el = optional_sets, set = shock_names))) {
        shock_ele <- shock_arg[["shock_ele"]][[1]]
        names(x = shock_ele) <- shock_arg[["shock_set"]][[1]]

        shock_arg <- shock_arg[!is.element(
          el = names(x = shock_arg),
          set = optional_sets
        )]

        shock_arg <- c(shock_arg, shock_ele)
      }
      return(shock_arg)
    }
  )

  # check that chosen shock variables exists within model
  lapply(X = shock_list,
         FUN = function(shk) {
           if (!is.element(
             el = tolower(x = shk[["var"]]),
             set = tolower(x = var_extract[["name"]])
           )) {
             stop(paste(dQuote(x = shk[["var"]]),
                        "was not found within the model Tablo file."))
           }

         })


  # multiple element condition
  multi_ele <- sapply(
    X = shock_list,
    FUN = function(component) {
      any(lapply(X = component, FUN = length) > 1)
    }
  )

  if (any(multi_ele)) {
    # pull out the multi_ele entries
    multi_shock <- shock_list[multi_ele]
    # drop the original undivided shock
    shock_list <- shock_list[!multi_ele]

    for (m_shk in seq_along(multi_shock)) {
      shk <- multi_shock[[m_shk]]
      # get set components
      shk_set <- shk[!is.element(el = names(x = shk), set = c("var", "type", "value", "file"))]

      exp_entries <- do.call(expand.grid, list(shk_set,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      ))
      expanded_entries <- lapply(seq_len(nrow(exp_entries)), function(i) {
        as.list(exp_entries[i, ])
      })

      # append other components
      shk_append <- shk[is.element(el = names(x = shk), set = c("var", "type", "value", "file"))]
      f_shks <- lapply(
        X = expanded_entries,
        FUN = function(entry) {
          c(shk_append, entry)
        }
      )
      # update shock list
      shock_list <- c(shock_list, f_shks)
    }
  }

  # add var set information
  shock_list <- lapply(X = shock_list,
                       FUN = function(shk) {
                         shk[["ls_upper_idx"]] <- .get_sets(var = shk[["var"]],
                                                            var_extract = var_extract,
                                                            type = "upper")

                         shk[["ls_mixed_idx"]] <- .get_sets(var = shk[["var"]],
                                                            var_extract = var_extract,
                                                            type = "mixed")
                         return(shk)
                       })

  return(shock_list)
}
