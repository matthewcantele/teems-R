#' @importFrom purrr pmap
#' 
#' @keywords internal
#' @noRd
.expand_closure <- function(closure,
                            var_extract,
                            sets) {
  # check that all variable names from closure are present within the var extract
  if (!all(is.element(el = tolower(x = closure[["var_name"]]), set = tolower(x = var_extract[["name"]])))) {
    var_discrepany <- tolower(x = closure[["var_name"]])[!is.element(
      el = tolower(x = closure[["var_name"]]),
      set = tolower(x = var_extract[["name"]])
    )]
    stop(paste(
      "The variable(s)",
      var_discrepany,
      "was/were not been located within the variable extract."
    ))
  }

  # structured data, overspecified control flow for now - revise
  closure[["struct"]] <- purrr::pmap(
    .l = with(
      data = closure,
      expr = list(var_name, full_var, subset_var, mixed_var, ele_var)
    ),
    .f = function(name, full, subsets, mixed, ele) {
      # pure element entries (e.g., qo("land","asa","1")
      if (!is.na(x = ele)) {
        .expand_var(
          set_names = .convert_var(concatenated_data = ele),
          set_elements = sets[["elements"]],
          sorted = FALSE
        )
      } else if (!is.na(x = mixed)) {
        # mixed qo(ENDWNC_COMM,"asa",alltime)
        .expand_var(
          set_names = .convert_var(concatenated_data = mixed),
          set_elements = sets[["elements"]],
          sorted = FALSE
        )
      } else if (!is.na(x = subsets)) {
        # partial var via subsets (e.g., qo(ENDWNC_COMM,REG,alltime))
        .expand_var(
          set_names = .convert_var(concatenated_data = subsets),
          set_elements = sets[["elements"]],
          sorted = FALSE
        )
      } else if (!is.na(x = full)) {
        # full var: afall
        .expand_var(
          var_names = full,
          set_elements = sets[["elements"]],
          var_extract = var_extract,
          sorted = FALSE
        )
      }
    }
  )

  names(x = closure[["struct"]]) <- closure[["var_name"]]
  return(closure)
}
