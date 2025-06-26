#' @importFrom tibble tibble
#' @importFrom purrr pluck pmap list_flatten map2 compact
#' @importFrom data.table CJ
#' 
#' @keywords internal
#' @noRd
.swap_out <- function(closure,
                      swap_out,
                      sets,
                      var_extract) {

  if (!is.null(x = swap_out)) {
    concat_swap_out <- lapply(
      X = swap_out,
      FUN = .check_swap,
      var_extract = var_extract,
      sets = sets
    )

    new_exits <- unlist(x = concat_swap_out)

    # create swap_out specific closure structure
    checked_exits <- .check_closure(
      closure = new_exits,
      sets = sets
    )

    expanded_exits <- .expand_closure(
      closure = checked_exits,
      var_extract = var_extract,
      sets = sets
    )

    # swap out preferentially from full var names to tuples
    # use for loop due to iterative nature of swaps
    # implement simultaneous swap feater
    # this is nightmarish, refactor!
    for (out_var in unique(x = expanded_exits[["var_name"]])) {
      if (!is.element(el = out_var, set = closure[["var_name"]])) {
        stop(paste("The variable:", out_var, "is not present in the underlying closure."))
      } else {
        # check status (full or not full swap)
        full_swap <- any(!is.na(x = unlist(x = subset(
          x = expanded_exits,
          subset = {
            is.element(
              el = var_name,
              set = out_var
            )
          },
          select = full_var
        ))))

        # partial swap condition
        if (!full_swap) {
          # get all possible variable entries in current closure
          current_var <- subset(
            x = closure,
            subset = {
              is.element(
                el = var_name,
                set = out_var
              )
            }
          )
          if (nrow(current_var) > 1) {
            stop("Extensions planned here. Contact the package maintainer.")
          }

          var_swap_outs <- purrr::pluck(subset(
            x = expanded_exits,
            subset = {
              is.element(el = var_name, set = out_var)
            },
            select = struct
          ), 1)

          var_swap_outs <- purrr::list_flatten(x = var_swap_outs)
          remaining_exo_var <- purrr::list_flatten(x = current_var[["struct"]])[[1]]

          for (sep_swap in 1:length(var_swap_outs)) {
            # check that all elements are present
            exo_to_endo <- var_swap_outs[[sep_swap]]

            el <- .convert_var(
              structured_data = exo_to_endo,
              var_name = out_var
            )
            set <- .convert_var(
              structured_data = remaining_exo_var,
              var_name = out_var
            )

            if (!all(is.element(el = el, set = set))) {
              stop(paste(
                "One or more swaps on the variable:",
                out_var,
                "overlap."
              ))
            }

            # remove elements to be endogenized
            remaining_exo_var <- setdiff(
              x = set,
              y = el
            )

            # back to struct
            ls_remaining <- lapply(
              X = remaining_exo_var,
              FUN = function(tuple) {
                .convert_var(
                  concatenated_data = tuple,
                  drop_quotes = TRUE
                )
              }
            )

            remaining_exo_var <- rbindlist(l = lapply(ls_remaining, function(x) as.list(x[[out_var]])))
          }
          remaining_exo_var[] <- mapply(
            FUN = as,
            remaining_exo_var,
            sapply(
              X = purrr::list_flatten(x = current_var[["struct"]])[[1]],
              FUN = class
            ),
            SIMPLIFY = FALSE
          )

          # create new set entry (for new set in tab)
          # out_ele <- swap_out[is.element(
          #   el = names(x = purrr::list_flatten(x = swap_out)),
          #   set = out_var
          # )]
          # 
          # out_ele <- purrr::list_flatten(x = out_ele)
          # 
          # var_concat_swap_out <- concat_swap_out[is.element(
          #   el = names(x = list_flatten(x = concat_swap_out)),
          #   set = out_var
          # )]

          # swap-wise diff (note previous remaining_exo_var is var-specific, this is swap-specific)
          # standard_var_sets <- .get_sets(
          #   var = out_var,
          #   var_extract = var_extract,
          #   type = "upper"
          # )

          standard_var_sets <- purrr::pluck(.x = var_extract, "ls_upper", out_var)
          colnames(x = remaining_exo_var) <- standard_var_sets

          # algo to pull out largest complete sets by element as possible
          # get set lengths
          set_ele <- with(data = sets[["mapped_ele"]], expr = mget(x = standard_var_sets))
          # set_length <- lengths(x = set_ele)

          # sets that have been swapped on
          swapped_sets <- setdiff(colnames(remaining_exo_var), colnames(var_swap_outs[[1]]))

          set_length <- sapply(
            X = swapped_sets,
            FUN = function(s) {
              set_length <- length(x = with(data = sets[["mapped_ele"]], expr = get(x = s)))
              return(set_length)
            }
          )

          swapped_sets <- swapped_sets[order(rank(x = set_length))]

          full_ss <- do.call(what = data.table::CJ, set_ele)

          # initialize
          out_ss <- list()
          struct_ss <- list()

          # no telling how robust this algo is - what a pain
          for (d in seq_along(swapped_sets)) {
            set_name <- swapped_sets[d]

            if (identical(x = d, y = 1L)) {
              full_ss <- split(x = full_ss, by = set_name)
              ss <- split(x = remaining_exo_var, by = set_name)

              # if set is completely missing
              if (!all(is.element(el = names(x = full_ss), set = names(x = ss)))) {
                null_sets <- full_ss[!is.element(el = names(x = full_ss), set = names(x = ss))]
                null_sets <- lapply(
                  X = null_sets,
                  FUN = function(s) {
                    return(NULL)
                  }
                )
                ss <- c(ss, null_sets)
                ss <- ss[match(x = names(x = full_ss), table = names(x = ss))]
              }
            } else {
              ss <- purrr::list_flatten(x = lapply(X = ss, FUN = split, by = set_name))
              full_ss <- purrr::list_flatten(x = lapply(X = full_ss, FUN = split, by = set_name))
              full_ss <- full_ss[is.element(el = names(x = full_ss), set = names(x = ss))]
            }

            incomplete <- unlist(x = purrr::map2(
              .x = full_ss,
              .y = ss,
              .f = function(target, current) {
                incomplete <- !isTRUE(
                  x = all.equal(
                    target,
                    current,
                    check.attributes = FALSE,
                    ignore.col.order = TRUE,
                    ignore.row.order = TRUE
                  )
                )
              }
            ))

            ss <- lapply(X = ss, FUN = function(subset) {
              if (!is.null(x = subset)) {
                replacement <- paste0("\"", unlist(x = unique(x = subset[, ..set_name])), "\"")
                colnames(x = subset) <- gsub(
                  pattern = set_name,
                  replacement = replacement,
                  x = colnames(x = subset)
                )
              }
              return(subset)
            })

            full_ss <- lapply(X = full_ss, FUN = function(subset) {
              if (!is.null(x = subset)) {
                replacement <- paste0("\"", unlist(x = unique(x = subset[, ..set_name])), "\"")
                colnames(x = subset) <- gsub(
                  pattern = set_name,
                  replacement = replacement,
                  x = colnames(x = subset)
                )
              }
              return(subset)
            })

            if (any(!incomplete)) {
              # extract
              struct_out <- ss[!incomplete]
              for (out in seq_along(struct_out)) {
                s_out <- struct_out[[out]]

                # log outs
                out_ss[length(x = out_ss) + 1] <- list(colnames(x = s_out))
                struct_ss[length(x = struct_ss) + 1] <- list(s_out)
              }
            }
            # update
            ss <- purrr::compact(.x = ss[incomplete])
            full_ss <- purrr::compact(.x = full_ss[incomplete])
          }

          new_entries <- sapply(
            X = out_ss,
            FUN = function(entry) {
              paste0(
                out_var,
                "(",
                paste0(entry, collapse = ","),
                ")"
              )
            }
          )
          # remove old entry
          closure <- subset(
            x = closure,
            subset = {
              !is.element(el = var_name, set = out_var)
            }
          )

          closure_append <- tibble::tibble(
            orig_closure = new_entries,
            var_name = out_var,
            full_var = NA,
            subset_var = NA,
            mixed_var = new_entries,
            ele_var = NA,
            struct = struct_ss
          )

          # new closure
          closure <- rbind(closure, closure_append)
        } else {
          # swap full var out
          closure <- subset(
            x = closure,
            subset = {
              !is.element(
                el = var_name,
                set = out_var
              )
            }
          )
        }
      }
    }
  }

  return(closure)
}
