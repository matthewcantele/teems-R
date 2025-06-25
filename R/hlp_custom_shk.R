#' @importFrom purrr pluck list_flatten map2_chr
#' @importFrom data.table fread CJ rbindlist setnames fsetdiff fsetequal
#' 
#' @keywords internal
.custom_shock <- function(raw_shock,
                          closure,
                          var_extract,
                          sets) {
  # user files are not being cached and so tracking is difficult
  # it could be possible to make modifications and targets won't know
  # hence tar_cue always on shock targets
  # set ele checks
  ls_upper <- purrr::pluck(.x = var_extract, "ls_upper_idx", raw_shock[["var"]])
  ls_mixed <- purrr::pluck(.x = var_extract, "ls_mixed_idx", raw_shock[["var"]])
  set_ele <- with(
    data = sets[["mapped_ele"]],
    expr = mget(x = ls_upper)
  )

  # not sure why this would be NULL, data.frames to be written to file
  if (is.null(x = raw_shock[["value"]])) {
    value <- data.table::fread(raw_shock[["file"]])
  } else {
    value <- raw_shock[["value"]]
  }

  template_shk <- do.call(
    what = data.table::CJ,
    args = c(set_ele, sorted = FALSE)
  )

  data.table::setnames(x = template_shk, new = ls_mixed)

  if (data.table::fsetequal(x = template_shk, y = value[,!"Value"])) {
    attr(x = raw_shock[["var"]], which = "full_var") <- TRUE

    # redundant code (used in uniform_shk)
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

    shock <- list(
      dt = value,
      var_name = raw_shock[["var"]],
      idx = .get_index(dt = value),
      type = raw_shock[["type"]]
    )
    
    shock <- list(shock)
    
  } else {
    if (!identical(x = nrow(x = data.table::fsetdiff(x = value[, !"Value"],
                                                     y = template_shk)),
                   y = 0L)) {
      errant_tuples <- data.table::fsetdiff(value[, !"Value"], template_shk)
      errant_tuples <- capture.output(print(x = errant_tuples))
      errant_tuples <- errant_tuples[-c(1, 2, 3)]
      .cli_action(msg = "Some tuples provided to a {.arg custom} shock indicate 
                  elements used that do not belong to their respective sets: 
                  {.field {errant_tuples}}.",
                  action = "abort")
    }
    attr(x = raw_shock[["var"]], which = "full_var") <- FALSE
    # Year to time conversion here
    if (raw_shock[["check_status"]]) {
      if (!is.element(el = raw_shock[["var"]], set = closure[["full_var"]])) {
        exo_parts <- purrr::list_flatten(x = subset(
          x = closure,
          subset = {
            is.element(el = var_name, set = raw_shock[["var"]])
          },
          select = struct
        )[[1]])
        
        all_exo_parts <- data.table::rbindlist(
          l = exo_parts,
          use.names = FALSE
        )
        
        key_names <- names(x = value[, !"Value"])
        data.table::setnames(x = all_exo_parts, new = key_names)
        if (!identical(x = nrow(x = data.table::fsetdiff(x = value[, !"Value"],
                                                         y = all_exo_parts)),
                       y = 0L)) {
          x_exo_parts <- data.table::fsetdiff(x = value[, ..key_names],
                                              y = all_exo_parts)
          x_exo_parts <- trimws(x = capture.output(print(x = x_exo_parts)))
          x_exo_parts <- x_exo_parts[-c(1, 2, 3, length(x = x_exo_parts))]
          .cli_action(msg = "Some tuples designated for a shock do not have 
                      exogenous status: {.field {x_exo_parts}}.",
                      action = "abort")
        }
      }
    }

    set_combn <- list()
    key_cols <- names(x = set_ele)
    for (size in 1:length(x = key_cols)) {
      col_combinations <- combn(x = key_cols,
                                m = size,
                                simplify = FALSE)
      set_combn <- c(set_combn, col_combinations)
    }
    
    set_combn <- set_combn[-length(x = set_combn)]
    
    shock <- list()
    shk_idx <- 1
    
    for (sel_comb in unique(x = set_combn)) {
      cycle_ele <- with(data = set_ele,
                        expr = mget(x = sel_comb))
      
      ele_combn <- do.call(data.table::CJ, cycle_ele)
      for(sel_ele in 1:nrow(x = ele_combn)) {
        ele <- unlist(x = ele_combn[sel_ele,])
        col_idx <- match(x = sel_comb, table = names(set_ele))
        col <- colnames(x = value)[col_idx]
        # get template subset
        dynamic_filter <- purrr::map2_chr(.x = col,
                    .y = ele,
                    .f = function(c, e) {
                      paste(c, "==", paste0("\"", e, "\""))
                    })
        
        if (length(x = dynamic_filter) > 1) {
          dynamic_filter <- paste(dynamic_filter, collapse = " & ")
        }
        
        ss_tmpl <- template_shk[eval(expr = parse(text = dynamic_filter))]
        ss_value <- value[eval(expr = parse(text = dynamic_filter))]
        
        if (all(is.element(el = ss_tmpl, set = ss_value))) {
          shk <- raw_shock
          attr(x = shk[["var"]], which = "full_var") <- TRUE
          shk[["value"]] <- ss_value
          value <- data.table::fsetdiff(x = value, y = ss_value)
          new_col_nme <- paste0("\"", ele, "\"")
          data.table::setnames(x = shk[["value"]], old = col, new = new_col_nme)
          shock[[shk_idx]] <- list(dt = shk[["value"]],
                                   var_name = shk[["var"]],
                                   idx = .get_index(dt = shk[["value"]]),
                                   type = raw_shock[["type"]])
          shk_idx <- shk_idx + 1
        }
      }
    }
    
    if (isTRUE(x = nrow(x = value) > 0)) {
      residual_shocks <- list(
        dt = value,
        var_name = raw_shock[["var"]],
        idx = .get_index(dt = value),
        type = raw_shock[["type"]]
      )

      shock <- c(shock, list(residual_shocks))
    } 
  }
  return(shock)
}