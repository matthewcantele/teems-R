#' @importFrom purrr pluck
#' @importFrom data.table as.data.table
#' 
#' @keywords internal
.coeff_generate <- function(name,
                            type,
                            tab_file,
                            ...) {

  call = match.call()
  extract <- .process_tablo(type = type,
                            tab_file = tab_file)
  
  c_names <- c(purrr::pluck(.x = extract, "ls_mixed_idx", name), "Value")
  comp_inputs <- list(...)
  
  if (!all(is.element(el = c_names, set = names(comp_inputs)))) {
    .cli_action(msg = c("One or more sets or the Value column is missing.",
                        "The required input columns for {.val {name}} are 
                {.val {names(comp_inputs)}}."),
                action = c("abort", "inform"))
  }

  if (!Reduce(function(a, b) identical(a, b),lengths(comp_inputs))) {
    .cli_action(msg = "Length of all provided inputs (passed with {.arg ...}) 
    must be equal in order to construct a data.frame.",
                action = "abort")
  }

  if (!all.equal(target = c_names, current = names(x = comp_inputs))) {
  e_idx <- match(x = names(x = comp_inputs), table = c_names)
  comp_inputs <- comp_inputs[e_idx]
  }
  
  gen_comp <- data.table::as.data.table(x = comp_inputs)
  attr(x = gen_comp, which = "name") <- name
  return(gen_comp)
}
