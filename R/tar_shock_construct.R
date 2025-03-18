#' @importFrom purrr pluck map2
#' @importFrom tibble tibble
#' 
#' @keywords internal
#' @noRd
.shock_construct <- function(shock_list,
                             closure,
                             var_extract,
                             sets,
                             param,
                             reference_year) {
  browser()
  final_shocks <- list()
  counter <- 0

  for (ls_shock in shock_list) {
    counter <- counter + 1
    shock_ID <- paste0(ls_shock[["var"]], counter)

    final_shocks[[shock_ID]] <- with(
      data = ls_shock,
      expr = {
        browser()
        fct_names <- names(x = ls_shock)

        if (is.element(el = type, set = c("scenario", "custom"))) {
          # if scenario shock, prep and convert to custom
          value <- data.table::fread(input = eval(expr = file))

          # make sure there's a value column
          if (!is.element(el = "Value", set = colnames(x = value))) {
            stop(paste(
              dQuote(x = "custom"),
              "and",
              dQuote("scenario"),
              "shocks require a 'Value' column."
            ))
          }
          browser()
          # convert to ALLTIMEt if only Year given
          if (is.element(el = "Year", set = colnames(x = value))) {
            AYRS <- purrr::pluck(.x = param, "dt", "AYRS")
            AYRS[["CYRS"]] <- reference_year + AYRS[["Value"]]
            if (!all(is.element(el = value[["Year"]], set = AYRS[["CYRS"]]))) {
              stop("One or more years provided within an intertemporal shock is not consistent with the implied years from time steps.")
            }
            r_idx <- match(x = value[["Year"]], AYRS[["CYRS"]])
            value[["ALLTIMEt"]] <- AYRS[["ALLTIMEt"]][r_idx]
            value <- value[, !"Year"]
          }

          if (identical(x = type, y = "scenario")) {
            value <- .convert_scenario(
              input = value,
              reference_year = reference_year,
              AYRS = purrr::pluck(.x = param, "dt", "AYRS"),
              sets = sets
            )
            # now it is effectively a type "custom" shock
            type <- "custom"
          }
        }

        # break into shock type specifc control flows
        if (identical(x = type, y = "uniform")) {
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
              stop(paste(
                "The variable",
                dQuote(x = var),
                "has been allocated a shock but is not identified as exogenous."
              ))
            }

            expanded_shk <- .expand_closure(
              closure = checked_shk,
              var_extract = var_extract,
              sets = sets
            )

            var_specific_exo <- purrr::list_flatten(x = subset(
              x = closure,
              subset = {
                is.element(el = var_name, set = var)
              },
              select = struct
            )[[1]])

            if (length(x = var_specific_exo) > 1) {
              var_specific_exo <- data.table::rbindlist(
                l = var_specific_exo,
                use.names = FALSE
              )
            }

            exo_list <- .convert_var(
              structured_data = var_specific_exo,
              var_name = var
            )

            shk_list <- .convert_var(
              structured_data = purrr::list_flatten(x = expanded_shk[["struct"]])[[1]],
              var_name = var
            )

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
          browser()
          # check that all tuples with entries are valid
          # construct data.table from var sets
          set_ele <- with(
            data = sets[["elements"]],
            expr = mget(x = ls_upper_idx)
          )
          
          constructed_dt <- do.call(
            what = data.table::CJ,
            args = c(set_ele, sorted = FALSE)
          )
          
          colnames(x = constructed_dt) <- ls_mixed_idx
          
          # check set order and reorder if necessary
          if (!identical(x = c(ls_mixed_idx, "Value"), y = colnames(x = value))) {
            data.table::setcolorder(x = value, neworder = c(ls_mixed_idx, "Value"))
          }
          
          custom_inputs <- apply(value[, !"Value"], 1, paste, collapse = ";")
          valid_inputs <- apply(constructed_dt, 1, paste, collapse = ";")
          
          if (!all(is.element(el = custom_inputs, set = valid_inputs))) {
            # reword and add output showing some that are incorrect
            stop("Custom shock to base data set elements mismatch detected.")
          }
          
          # check for full_var status
          if (all(is.element(el = valid_inputs, set = custom_inputs))) {
            attr(x = var, which = "full_var") <- TRUE
          } else {
            attr(x = var, which = "full_var") <- FALSE
          }
          
          # check that var has a closure entry
          if (!is.element(el = var, set = closure[["full_var"]])) {
            stop(paste(var, "does not have full exogenous status."))
          }
          
          # check closure status
          var_specific <- purrr::list_flatten(x = subset(
            x = closure,
            subset = {
              is.element(el = var_name, set = var)
            },
            select = struct
          )[[1]])
          
          var_specific <- data.table::rbindlist(
            l = var_specific,
            use.names = FALSE
          )

          valid_inputs <- apply(var_specific, 1, paste, collapse = ";")
          
          if (!all(is.element(el = custom_inputs, set = valid_inputs))) {
            # reword and add output showing some that are incorrect
            stop("Custom shock entires not exogenous.")
          }
          
          shock <- list(
            dt = value,
            var_name = var,
            idx = .get_index(dt = value),
            type = type
          )
          
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
