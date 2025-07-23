.prep_closure <- function(closure,
                          var_omit,
                          var_extract,
                          sets) {

  checked_cls <- .classify_entries(
    closure = closure,
    sets = sets
  )
  
  expanded_cls <- .expand_closure(
    closure = checked_cls,
    var_omit = var_omit,
    var_extract = var_extract,
    sets = sets
  )
  
  return(expanded_cls)
}