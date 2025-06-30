#' @importFrom purrr pluck
#' 
#' @keywords internal
.check_shock <- function(shock,
                         var_extract,
                         int_sets = NULL,
                         value_mod = FALSE,
                         call) {
  browser()
  if (!.is_shock(x = shock)) {
    if (!.is_shock(x = shock) || !.is_shock(x = shock[[1]])) {
      .cli_action(
        msg = "The value provided to {.arg shock} is not an object
            created with {.fun teems::teems_shock}.",
        action = "abort",
        call = call
      )
    } else {
      shock <- shock[[1]]
    }
  }

  if (!is.element(el = shock[["var"]], set = var_extract[["name"]])) {
    var_name <- shock[["var"]]
    .cli_action(
      msg = "The variable designated for a shock: {.val {var_name}}
                was not found within the model Tablo file.",
      action = "abort",
      call = call
    )
  }

  if (!is.null(x = shock[["set"]])) {
    ls_mixed <- purrr::pluck(.x = var_extract, "ls_mixed_idx", shock[["var"]])
    if (!is.null(x = int_sets)) {
      if (any(grepl(pattern = "Year", x = shock[["set"]]))) {
        int_sets <- paste0(int_sets, "t")
        if (any(is.element(el = int_sets, shock[["set"]]))) {
          supplied_int_set <- intersect(x = shock[["set"]], y = int_sets)
          .cli_action(
            msg = "If {.field Year} is provided in lieu of the
                      intertemporal set, the intertemporal set
                      {.field {supplied_int_set}} is not necessary.}",
            action = "abort",
            call = call
          )
        }
        ls_mixed <- c(setdiff(x = ls_mixed, int_sets), "Year")
      }
    }
    if (!all(is.element(el = shock[["set"]], set = ls_mixed))) {
      errant_set <- setdiff(x = shock[["set"]], y = ls_mixed)
      var_name <- shock[["var"]]
      .cli_action(
        msg = c(
          "The set designated for an element-specific shock:
                  {.val {errant_set}} is not associated with the variable
                  {.val {var_name}}.",
          "Note that set designations within {.pkg teems} are
                          comprised of the variable-specific uppercase set name and
                          lowercase index.",
          "For {.val {var_name}} these include: {.field {ls_mixed}}.",
          "For intertemporal models, {.field Year} may be
                          provided in lieu of an intertemporal set."
        ),
        action = c("abort", "inform", "inform", "inform"),
        call = call
      )
    }
    
    if (is.element(el = shock[["type"]], set = c("custom", "scenario"))) {
      if (!all(is.element(el = ls_mixed, set = shock[["set"]]))) {
        missing_sets <- setdiff(x = ls_mixed, y = shock[["set"]])
        var <- shock[["var"]]
        type <- shock[["type"]]
        .cli_action(
          msg = c(
            "A {.arg type} shock input must include as columns
        all sets associated with the variable {.val {var}}.",
            "Current input is missing {.field {missing_sets}}."
          ),
          action = c("abort", "inform"),
          call = call
        )
      }

      if (!identical(x = ls_mixed, y = shock[["set"]])) {
        value <- data.table::fread(input = shock[["value"]])
        data.table::setcolorder(x = value, neworder = c(ls_mixed, "Value"))
        value_mod <- TRUE
      }
      
      if (identical(x = shock[["type"]], y = "scenario")) {
        if (is.null(x = int_sets)) {
          .cli_action(msg = "Shock type {.arg scenario} is only valid for 
                      temporally dynamic models.",
                      action = "abort",
                      call = call)
        }
        if (!any(is.element(el = "Year", set = value_colnames))) {
          .cli_action(msg = "Shock type {.arg scenario} must include a
                      {.field Year} column associated with each time step as 
                      determined by {.fun teems::teems_time}.",
                      action = "abort",
                      call = call)
        }
      }
    }
  }
  
  # modified shock from file/data.frame cached
  if (!is.null(x = shock[["file"]]) && value_mod) {
    data.table::fwrite(x = value,
                       file = shock[["file"]])
  }

  # better to keep this here rather than chk_shocK_input due to list shenanigans
  if (any(lengths(x = shock[["ele"]]) > 1)) {
    ele_comb <- expand.grid(shock[["ele"]], stringsAsFactors = FALSE)

    shock <- purrr::map(
      .x = seq_len(nrow(ele_comb)),
      .f = function(c) {
        new_list <- shock
        for (set_name in shock[["set"]]) {
          new_list[["ele"]][[set_name]] <- ele_comb[[set_name]][c]
        }
        return(new_list)
      }
    )
  } else {
    shock <- list(shock)
  }

  return(shock)
}
