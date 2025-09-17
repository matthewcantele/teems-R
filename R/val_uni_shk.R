#' @keywords internal
#' @noRd
.val_uni_shk <- function(shock,
                         call) {

  checklist <- list(
    var = "character",
    type = "character",
    input = "numeric",
    subset = c("NULL", "list")
  )

  .check_arg_class(
    args_list = shock,
    checklist = checklist,
    call = call
  )

  shock <- structure(shock,
    call = call,
    class = c(shock$type, "shock", class(shock))
  )
  
  shock <- list(shock)
  return(shock)
}