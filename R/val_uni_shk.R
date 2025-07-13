#' @importFrom uuid UUIDgenerate
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
  # eventually creating an R6 class for shock objects is probably ideal
  args_list <- structure(args_list,
                         call_id = uuid::UUIDgenerate(),
                         call = call,
                         shock = TRUE,
                         class = c(args_list$type, "list"))
  args_list <- list(args_list)
  return(args_list)
}