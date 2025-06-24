#' @importFrom purrr list_flatten
#'
#' @export
teems_data <- function(dat_input,
                       par_input,
                       aux_input = NULL,
                       aggregated_data = NULL,
                       unaggregated_data = NULL,
                       quiet = FALSE)
{
call <- match.call()
args_list <- mget(x = names(x = formals()))
args_list <- .validate_data_args(args_list = args_list,
                                 call = call,
                                 quiet = quiet)
args_list[["dat_input"]] <- .check_input(file = dat_input,
                                         valid_ext = c("har", "qs2"),
                                         call = call)
metadata <- .get_metadata(con = args_list[["dat_input"]])
.check_database_version(vetted =  c("v9A", "v10A", "v11c"),
                        provided = metadata[["full_database_version"]],
                        call = call,
                        quiet = quiet)
args_list[["par_input"]] <- .check_input(file = par_input,
                                         valid_ext = c("har", "qs2"),
                                         call = call)
if (!is.null(x = aux_input)) {
  args_list[["aux_input"]] <- .check_input(file = aux_input,
                                           valid_ext = c("har", "qs2"),
                                           call = call)
}
.inform_metadata(metadata = metadata,
                 quiet = quiet)
config <- c(args_list,
            metadata = list(metadata),
            call = call)
config
}
