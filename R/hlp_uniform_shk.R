#' @importFrom purrr pluck map2 list_flatten
#' @importFrom data.table rbindlist
#'
#' @keywords internal
.uniform_shock <- function(raw_shock,
                           closure,
                           var_extract,
                           sets) {
  ls_upper <- purrr::pluck(.x = var_extract, "ls_upper_idx", raw_shock[["var"]])
  if (is.null(x = raw_shock[["set"]])) {
    attr(x = raw_shock[["var"]], which = "full_var") <- TRUE
    if (raw_shock[["check_status"]]) {
      if (!is.element(el = raw_shock[["var"]], set = closure[["full_var"]])) {
        var_name <- raw_shock[["var"]]
        .cli_action(
          msg = "The variable {.val {var_name}} was assigned a uniform
                  shock over the entire variable yet is not fully exogenous.",
          action = "abort"
        )
      }
    }
    if (!identical(x = ls_upper, y = "null_set")) {
      shock_LHS <- paste0(raw_shock[["var"]], "(", paste0(ls_upper, collapse = ","), ")")
    } else {
      shock_LHS <- raw_shock[["var"]]
    }
  } else {
    attr(x = raw_shock[["var"]], which = "full_var") <- FALSE
    if (is.element(el = "Year", set = raw_shock[["set"]])) {
      # check here on whether year is within range of time_set
      ls_mixed <- purrr::pluck(.x = var_extract, "ls_mixed_idx", raw_shock[["var"]])
      time_set <- setdiff(x = ls_mixed, y = raw_shock[["set"]])
      CYRS <- attr(x = sets, which = "CYRS")
      v_idx <- match(x = purrr::pluck(.x = raw_shock, "ele", "Year"), CYRS)
      purrr::pluck(.x = raw_shock, "ele", "Year") <- v_idx
      raw_shock[["set"]] <- sub(pattern = "Year", replacement = time_set, x = raw_shock[["set"]])
      names(x = raw_shock[["ele"]]) <- raw_shock[["set"]]
    }
    upper_nmes <- sub(
      pattern = ".{1}$",
      replacement = "",
      x = names(x = raw_shock[["ele"]])
    )

    purrr::map2(
      .x = raw_shock[["ele"]],
      .y = upper_nmes,
      .f = function(ele, ele_set) {
        recognized_ele <- purrr::pluck(.x = sets, "mapped_ele", ele_set)

        if (!is.element(el = ele, set = recognized_ele)) {
          depurred.ele <- ele
          depurred.ele_set <- ele_set
          .cli_action(
            msg = c(
              "The element: {.val {depurred.ele}} is not found
                      within the associated set: {.val {depurred.ele_set}}",
              "Accepted elements with the current mapping
                              include: {.val {recognized_ele}}."
            ),
            action = c("abort", "inform")
          )
        }
      }
    )

    r_idx <- match(x = upper_nmes, table = ls_upper)
    ls_upper[r_idx] <- paste0('"', raw_shock[["ele"]], '"')
    shock_LHS <- paste0(raw_shock[["var"]], "(", paste0(ls_upper, collapse = ","), ")")

    if (raw_shock[["check_status"]]) {
      # name change this function, nothing checked
      checked_shk <- .check_closure(
        closure = shock_LHS,
        sets = sets
      )

      expanded_shk <- .expand_closure(
        closure = checked_shk,
        var_extract = var_extract,
        sets = sets,
        var_omit = NULL
      )

      var_specific_exo <- purrr::list_flatten(x = subset(
        x = closure,
        subset = {
          is.element(el = var_name, set = raw_shock[["var"]])
        },
        select = struct
      )[[1]])

      if (length(x = var_specific_exo) > 1) {
        var_specific_exo <- data.table::rbindlist(
          l = var_specific_exo,
          use.names = FALSE
        )
      }

      # still doing element-wise checks here, probably not efficient
      exo_list <- .convert_var(
        structured_data = var_specific_exo,
        var_name = raw_shock[["var"]]
      )

      shk_list <- .convert_var(
        structured_data = purrr::list_flatten(x = expanded_shk[["struct"]])[[1]],
        var_name = raw_shock[["var"]]
      )

      if (!all(is.element(el = shk_list, set = exo_list))) {
        errant_tup <- setdiff(x = shk_list, y = exo_list)
        .cli_action(
          msg = "The following tuples have been allocated a shock but
                  are not identified as exogenous: {.val {errant_tup}}.",
          action = "abort"
        )
      }
    }
  }
  shock_RHS <- paste("=", "uniform", paste0(raw_shock[["value"]], ";", "\n"))

  shock <- list(
    shock = paste("Shock", shock_LHS, shock_RHS),
    type = "uniform"
  )
  f_shock <- list(shock)
  return(f_shock)
}
