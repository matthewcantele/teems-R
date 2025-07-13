#' @export
.construct_shock.uniform <- function(raw_shock,
                                     closure,
                                     var_extract,
                                     sets) {
  if (attr(raw_shock, "full_var")) {
    if (.o_check_shock_status()) {
      if (!raw_shock$var %in% closure$full_var) {
        # if parts of a var are provided that comprise the full, not sure if caught
        # provide some endo components as examples here
        var_name <- raw_shock$var
        error_fun <- substitute(.cli_action(
          shk_err$x_full_exo,
          action = "abort",
          call = call
        ))
        
        error_var <- substitute(variables <- list(var_name = var_name))
        error_inputs <- .pipeline_error(
          error_var = error_var,
          error_fun = error_fun,
          call_id = attr(raw_shock, "call_id")
        )
        
        rlang::abort(error_inputs)
      }
    }
    
    if (!raw_shock$ls_upper %=% "null_set") {
      shock_LHS <- paste0(raw_shock$var, "(", paste0(ls_upper, collapse = ","), ")")
    } else {
      shock_LHS <- raw_shock$var
    }
  } else {
    if ("Year" %in% names(raw_shock$subset)) {
      Year <- purrr::pluck(raw_shock, "subset", "Year")
      time_set_upper <- intersect(raw_shock$ls_upper, subset(sets, intertemporal, name)[[1]])
      time_set <- raw_shock$ls_mixed[match(time_set_upper, raw_shock$ls_upper)]
      CYRS <- attr(sets, "CYRS")
      if (!Year %in% CYRS) {
        error_fun <- substitute(.cli_action(
          shk_err$uni_invalid_year,
          action = "abort",
          call = call
        ))
        
        error_var <- substitute(variables <- list(
          Year = Year,
          CYRS = CYRS
        ))
        error_inputs <- .pipeline_error(
          error_var = error_var,
          error_fun = error_fun,
          call_id = attr(raw_shock, "call_id")
        )
        
        rlang::abort(error_inputs)
      }
      v_idx <- match(Year, CYRS)
      purrr::pluck(raw_shock, "subset", "Year") <- purrr::pluck(sets, "mapped_ele", time_set_upper, v_idx)
      names(raw_shock$subset) <- sub("Year", time_set, names(raw_shock$subset))
    }
    
    upper_ss <- .dock_tail(names(raw_shock$subset))
    error_inputs <- purrr::map2(
      raw_shock$subset,
      upper_ss,
      function(ele, ele_set) {
        recognized_ele <- purrr::pluck(sets, "mapped_ele", ele_set)
        if (!ele %in% recognized_ele) {
          depurred.ele <- ele
          depurred.ele_set <- ele_set
          error_fun <- substitute(.cli_action(
            shk_err$uni_invalid_ele,
            action = c("abort", "inform"),
            call = call
          ))
          
          error_var <- substitute(variables <- list(
            depurred.ele = depurred.ele,
            depurred.ele_set = depurred.ele_set,
            recognized_ele = recognized_ele
          ))
          
          error_inputs <- .pipeline_error(
            error_var = error_var,
            error_fun = error_fun,
            call_id = attr(raw_shock, "call_id")
          )
          
          return(error_inputs)
        }
      }
    )
    
    if (!all(purrr::map_lgl(error_inputs, is.null))) {
      error_inputs <- purrr::compact(error_inputs)
      rlang::abort(error_inputs[[1]])
    }
    
    r_idx <- match(upper_ss, raw_shock$ls_upper)
    shock_LHS <- raw_shock$ls_upper
    shock_LHS[r_idx] <- paste0('"', raw_shock$subset, '"')
    shock_LHS <- paste0(raw_shock$var, "(", paste0(shock_LHS, collapse = ","), ")")
    
    if (.o_check_shock_status()) {
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
      
      var_specific_exo <- purrr::list_flatten(subset(
        closure,
        var_name %in% raw_shock$var,
        select = struct
      )[[1]])
      
      if (length(var_specific_exo) > 1) {
        var_specific_exo <- data.table::rbindlist(
          l = var_specific_exo,
          use.names = FALSE
        )
      }
      
      # still doing element-wise checks here, probably not efficient
      exo_list <- .convert_var(
        structured_data = var_specific_exo,
        var_name = raw_shock$var
      )
      
      shk_list <- .convert_var(
        structured_data = purrr::list_flatten(expanded_shk$struct)[[1]],
        var_name = raw_shock$var
      )
      
      if (!all(shk_list %in% exo_list)) {
        errant_tup <- setdiff(shk_list, exo_list)
        error_fun <- substitute(.cli_action(
          shk_err$x_part_exo,
          action = "abort",
          call = call
        ))
        error_var <- substitute(variables <- list(errant_tup = errant_tup))
        error_inputs <- .pipeline_error(
          error_var = error_var,
          error_fun = error_fun,
          call_id = attr(raw_shock, "call_id")
        )
        
        rlang::abort(error_inputs)
      }
    }
  }
  
  shock_RHS <- paste("=", "uniform", paste0(raw_shock$input, ";", "\n"))
  shock <- list(shock = paste("Shock", shock_LHS, shock_RHS))
  shock <- structure(shock,
                     class = class(raw_shock),
                     full_var = attr(raw_shock, "full_var"))
  
  shock <- list(shock)
  return(shock)
}