#' @importFrom stats setNames
#' @importFrom rlang caller_env fn_env
#' @importFrom utils URLencode
#' @importFrom cli cli_abort cli_inform cli_warn
#' 
#' @keywords internal
#' @noRd
.cli_action <- function(...,
                        action,
                        msg,
                        url = NULL,
                        hyperlink = NULL,
                        call = NULL) {

  symbol <- switch(EXPR = action,
                   "abort" = "x",
                   "inform" = "i",
                   "warn" = "!")
  
  formatted_msg <- c(stats::setNames(object = msg, nm = symbol))
  caller_env <- rlang::caller_env()

  if (!missing(...)) {
    append_text <- unlist(...)
    append_msg <- c("i" = append_text)
    formatted_msg <- c(formatted_msg, append_msg)
  }
  
  if (!is.null(x = url)) {
    url <- utils::URLencode(URL = url)
    url_msg <- c("i" = "For additional information see: {.href [{hyperlink}]({url})}")
    formatted_msg <- c(formatted_msg, url_msg)
    cli_env <- new.env(parent = caller_env)
    # we could add a no hyperlink option but hyperlinks are preferred 
    cli_env[["hyperlink"]] <- hyperlink
    cli_env[["url"]] <- url
  } else {
    cli_env <- caller_env
  }
  
  if (is.null(x = call)) {
    call <- caller_env
  }

  if (action == "abort") {
    cli::cli_abort(message = formatted_msg, call = call, .envir = cli_env)
  } else if (action == "inform") {
    cli::cli_inform(message = formatted_msg, call = call, .envir = cli_env)
  } else if (action == "warn") {
    cli::cli_warn(message = formatted_msg, call = call, .envir = cli_env)
  }
}
