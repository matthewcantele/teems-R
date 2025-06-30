#' @importFrom rlang current_env
#' 
#' @keywords internal
#' @noRd
.val_uni_shk <- function(args_list,
                         call) {
  checklist <- list(
    var = "is.character",
    type = "is.character",
    input = "is.numeric"
  )
  .check_arg_class(args_list = args_list[!names(args_list) %in% "subset"],
                   checklist = checklist,
                   call = call)
  args_list <- structure(args_list,
                         timestamp = Sys.time(),
                         class = "shock")
  args_list <- list(args_list)
  return(args_list)
}