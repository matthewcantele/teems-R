#' @importFrom purrr map2 pluck
#' 
#' @keywords internal
#' @noRd
# .check_swap <- function(swap,
#                         var_extract,
#                         sets) {
# browser()
#   if (!swap$swap_sets %=% NA) {
#     ls_upper_names <- purrr::pluck(var_extract, "ls_upper_idx", swap$var)
#     components <- purrr::pmap(
#       list(swap$swap_sets,
#            ls_upper_names,
#            names(swap$swap_sets)),
#       function(ele, set, mixed_set) {
#         browser()
#         if (!identical(x = ele, y = mixed_set)) {
#           set_elements <- with(data = sets[["mapped_ele"]], expr = get(x = set))
#           if (any(!is.element(el = ele, set = set_elements))) {
#             errant_ele <- ele[!is.element(el = ele, set = set_elements)]
#             depurrr_mixed_set <- mixed_set
#             
#             error_fun <- substitute(expr = .cli_action(
#               msg = "The set {.val {depurrr_mixed_set}} does not include the 
#               specified element-specific swap on: {.val {errant_ele}}.",
#               action = "abort",
#               call = model_call
#             ))
# 
#             error_var <- substitute(variables <- list(
#               depurrr_mixed_set = depurrr_mixed_set,
#               errant_ele = errant_ele
#             ))
#             
#             error_inputs <- .package_error(
#               error_var = error_var,
#               error_fun = error_fun
#             )
#             stop(message = error_inputs)
#           }
#           
#           # add quotes for elements
#           ele <- paste0("\"", ele, "\"")
#           return(ele)
#         } else {
#           return(set)
#         }
#       }
#     )
# 
#     if (any(lapply(X = components, FUN = length) > 1)) {
#       exp_components <- expand.grid(components, stringsAsFactors = FALSE)
#       # apply the concatenation function to each combination
#       concat_swap <- apply(
#         X = exp_components,
#         MARGIN = 1,
#         FUN = function(row) {
#           paste0(swap[["var"]], "(", paste(row, collapse = ","), ")")
#         }
#       )
#     } else {
#       concat_swap <- paste0(swap[["var"]], "(", paste(components, collapse = ","), ")")
#     }
#     checked_swap <- concat_swap
#   } else {
#     checked_swap <- swap[["var"]]
#   }
#   return(checked_swap)
# }