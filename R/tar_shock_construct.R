#' ShockCheck function
#'
#' This function checks the validity of shocks for a model run. It verifies that
#' shock variables exist within the model, are assigned exogenous status, and
#' that any set elements specified for shocks indeed belong to the set.
#'
#' @param shock_list Shocks to be checked.
#' @param closure A vector of variables assigned exogenous status.
#' @param var_extract A data frame containing the model extract.
#' @param sets A data frame of set information.
#' @param time_coeff Time related data
#' @note if a set error occurs due to choice of subindex REGr vs REGs, provide
#'   more explicit error message
#' @note provide var name for errors on set checks
#' @note future iterations allow for custom shock over entire variable with
#'   granular exo/endo status
#' @note single shocks not working
#'
#' @importFrom purrr pluck map2
#' @importFrom tibble tibble
#' @return A list containing the original shocks and the name of the shock file.
#' @keywords internal
#' @noRd
.shock_construct <- function(shock_list,
                             closure,
                             var_extract,
                             sets,
                             time_coeff) {
  # initialize list (we could preallocate memory but performance gain is minimal)
  final_shocks <- list()
  counter <- 0

  for (ls_shock in shock_list) {
    counter <- counter + 1
    shock_ID <- paste0(ls_shock[["var"]], counter)

    final_shocks[[shock_ID]] <- with(
      data = ls_shock,
      expr = {

        fct_names <- names(x = ls_shock)

        if (is.element(el = type, set = c("scenario", "custom"))) {
          # if scenario shock, prep and convert to custom
          value <- data.table::fread(input = eval(expr = file))

          # make sure there's a value column
          if (!is.element(el = "Value", set = colnames(x = value))) {
            stop(paste(dQuote(x = "custom"),
                       "and",
                       dQuote("scenario"),
                       "shocks require a 'Value' column."))
          }

          if (identical(x = type, y = "scenario")) {
            value <- .convert_scenario(input = value,
                                       time_coeff = time_coeff,
                                       sets = sets)
            # now it is effectively a type "custom" shock
            type <- "custom"
          }
        }

        if (is.element(el = type, set = c("uniform", "custom"))) {
          # get implied sets and check user-provided
          user_sets <- fct_names[!is.element(
            el = fct_names,
            set = c("var", "type", "value", "file", "ls_upper_idx", "ls_mixed_idx")
          )]
          # var status
          if (identical(x = user_sets, y = character(0))) {
            attr(x = var, which = "full_var") <- TRUE
          } else {
            attr(x = var, which = "full_var") <- FALSE
          }

          if (identical(x = type, y = "custom")) {
            explicit_sets <- colnames(x = value)[!is.element(
              el = colnames(x = value),
              set = "Value"
            )]
            # check for element specific components
            # label implicit set if given
            if (!identical(x = user_sets, y = character(0))) {
              user_sets <- c(user_sets, explicit_sets)
              implicit_sets <- user_sets[!is.element(el = user_sets, set = explicit_sets)]
            } else {
              implicit_sets <- NA
            }
          }

          if (!all(is.element(el = user_sets, set = ls_mixed_idx))) {
            errant_sets <- user_sets[!is.element(el = user_sets, set = ls_mixed_idx)]
            stop(paste(
              errant_sets,
              "set provided are not associated with",
              paste0(dQuote(x = var), "."),
              "Recognized sets include:",
              paste(ls_mixed_idx, collapse = ", ")
            ))
          }
        }

        # break into shock type specifc control flows
        if (identical(x = type, y = "uniform")) {
          # check that any elements belong to the associated sets
          user_set_ele <- mget(x = user_sets)

          # convert to standard
          names(x = user_set_ele) <- sub(pattern = ".{1}$", replacement = "", x = user_sets)

          purrr::map2(
            .x = user_set_ele,
            .y = names(x = user_set_ele),
            .f = function(ele, ele_set) {
              recognized_ele <- purrr::pluck(.x = sets, "elements", ele_set)

              if (!all(is.element(el = ele, set = recognized_ele))) {
                stop(paste(
                  "The element:",
                  ele,
                  "is not found within set:",
                  paste0(ele_set, "."),
                  "Current elements include:",
                  paste(recognized_ele, collapse = ", ")
                ))
              }
            }
          )

          # check exogenous status, construct final format LHS shock call
          r_idx <- match(
            x = names(x = user_set_ele),
            table = ls_upper_idx
          )

          ls_upper_idx[r_idx] <- paste0('"', user_set_ele, '"')
          shock_LHS <- paste0(var, "(", paste0(ls_upper_idx, collapse = ","), ")")

          # expand any sets
          checked_shk <- .check_closure(
            closure = shock_LHS,
            sets = sets
          )

          # non-numeriare (null_set) condition
          if (!identical(x = ls_upper_idx, y = "null_set")) {
            # This check needs to be shock type agnostic
            # full variable not exogenous
            if (!is.element(el = var, set = closure[["var_name"]])) {
              stop(paste("The variable",
                         dQuote(x = var),
                         "has been allocated a shock but is not identified as exogenous."))
            }

            expanded_shk <- .expand_closure(
              closure = checked_shk,
              var_extract = var_extract,
              sets = sets
            )

            var_specific_exo <- purrr::list_flatten(x = subset(x = closure,
                                       subset = {is.element(el = var_name, set = var)},
                                       select = struct)[[1]])

            if (length(x = var_specific_exo) > 1) {
              var_specific_exo <- data.table::rbindlist(l = var_specific_exo,
                                                        use.names = FALSE)
            }

            exo_list <- .convert_var(structured_data = var_specific_exo,
                                     var_name = var)

            shk_list <- .convert_var(structured_data = purrr::list_flatten(x = expanded_shk[["struct"]])[[1]],
                                     var_name = var)

            if (!all(is.element(el = shk_list, set = exo_list))) {
              stop(cat("The following tuples have been allocated a shock but are not identified as exogenous:",
                shk_list[is.element(el = shk_list, set = exo_list)],
                sep = "\n"
              ))
            }

            # shock RHS
            shock_RHS <- paste("=", "uniform", paste0(value, ";", "\n"))
          } else {
            shock_LHS <- sub(pattern = "\\(null_set\\)", replacement = "", x = shock_LHS)
            shock_RHS <- paste("=", paste0(value, ";", "\n"))
          }

          shock <- list(
            shock = paste("Shock", shock_LHS, shock_RHS),
            type = type
          )

          return(shock)
        } else if (identical(x = type, y = "custom")) {
          # modify if some sets are implicit
          if (!identical(x = implicit_sets, y = NA)) {
            ls_mixed_idx <- ls_mixed_idx[!is.element(el = ls_mixed_idx, set = implicit_sets)]
            ls_upper_idx <- substring(text = ls_mixed_idx, first = 1, last = nchar(x = ls_mixed_idx) - 1)
          }

          # if a set is completely omitted, expand to include the uniform set
          if (!all(is.element(el = ls_mixed_idx, set = colnames(x = value[, !"Value"])))) {
            missing_sets <- ls_mixed_idx[!is.element(el = ls_mixed_idx, set = colnames(x = value[, !"Value"]))]
            missing_sets <- substring(text = missing_sets, first = 1, last = nchar(x = missing_sets) - 1)
            m_set_ele <- with(data = sets[["elements"]], expr = mget(x = missing_sets))

            # expand on non-value columns
            x_value <- do.call(what = CJ, c(m_set_ele, value[, !"Value"]))
            colnames(x = x_value) <- ls_mixed_idx

            # merge on the previous to bring over values
            common_sets <- intersect(x = colnames(x = value[, !"Value"]), y = colnames(x = x_value))
            value <- data.table::merge.data.table(
              x = x_value,
              y = value,
              by = common_sets
            )
          }

          # check set order and reorder if necessary
          if (!identical(x = c(ls_mixed_idx, "Value"), y = colnames(x = value))) {
            data.table::setcolorder(x = value, neworder = c(ls_mixed_idx, "Value"))
          }

          # check that all tuples are present
          # construct data.table from var sets
          set_ele <- with(
            data = sets[["elements"]],
            expr = mget(x = ls_upper_idx)
          )

          constructed_dt <- do.call(
            what = data.table::CJ,
            args = c(set_ele, sorted = FALSE)
          )

          colnames(x = constructed_dt) <- colnames(value[, !"Value"])

          if (!isTRUE(x = (all.equal(
            current = value[, !"Value"],
            target = constructed_dt,
            ignore.row.order = TRUE,
            check.attributes = FALSE
          )))) {
            stop("Custom shock to base data set elements mismatch detected.")
          }

          # check variable status (must be full var)
          if (attr(x = var, which = "full_var")) {
            if (!is.element(el = var, set = closure[["full_var"]])) {
              stop(paste(var, "does not have full exogenous status."))
            }
            shock <- list(
              dt = value,
              var_name = var,
              idx = .get_index(dt = value),
              type = type
            )
          } else {
            # combine with implicit set for a check on the closure status
            # construct full closure entries
            implied_comp <- mget(x = implicit_sets)
            # convert names to standard (write an aux script for this!)
            names(x = implied_comp) <- substring(
              text = names(x = implied_comp),
              first = 1,
              last = nchar(x = names(x = implied_comp)) - 1
            )

            full_comp <- c(implied_comp, set_ele)

            # order check (refresh these as they have been modified)
            ls_upper_idx <- purrr::pluck(.x = var_extract, "ls_upper_idx", var)
            full_comp <- full_comp[match(x = ls_upper_idx, table = names(x = full_comp))]

            struct <- list(do.call(
              what = data.table::CJ,
              args = c(full_comp, sorted = FALSE)
            ))

            names(x = struct) <- var

            concat <- .convert_var(
              structured_data = struct,
              var_name = names(x = struct)
            )

            # var specific exo
            var_specific_exo <- purrr::list_flatten(x = subset(x = closure,
                                                               subset = {is.element(el = var_name, set = var)},
                                                               select = struct)[[1]])

            if (length(x = var_specific_exo) > 1) {
              var_specific_exo <- data.table::rbindlist(l = var_specific_exo,
                                                        use.names = FALSE)
            }

            exo_list <- .convert_var(structured_data = var_specific_exo,
                                     var_name = var)

            if (!all(is.element(el = concat, set = exo_list))) {
              stop(cat("The following tuples have been allocated a shock but are not identified as exogenous:",
                concat[is.element(el = concat, set = exo_list)],
                sep = "\n"
              ))
            }

            var_comp <- names(x = full_comp)
            r_idx <- match(x = names(x = implied_comp), table = var_comp)
            var_comp[r_idx] <- paste0("\"", implied_comp, "\"")

            # construct shock LHS
            shock_LHS <- paste0(var, "(", paste0(var_comp, collapse = ","), ")")

            shock <- list(
              dt = value,
              var_name = shock_LHS,
              idx = .get_index(dt = value),
              type = type
            )
            attr(x = shock[["var_name"]], which = "full_var") <- FALSE
          }
          return(shock)
        }
      }
    )
  }

  # get name for shock input
  if (length(x = final_shocks) < 4) {
    shock_id <- paste(substring(text = names(x = final_shocks), first = 1, last = 3), collapse = "_")
  } else {
    shock_id <- paste(substring(text = names(x = final_shocks), first = 1, last = 1), collapse = "")
  }

  shock_file <- paste0(paste(paste(shock_id, format(x = Sys.time(), "%H%M%S"), sep = "_"), collapse = "_"), ".shf")

  return(list(
    shocks = final_shocks,
    shock_file = shock_file
  ))
}
