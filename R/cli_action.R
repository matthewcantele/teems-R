#' @importFrom stats setNames
#' @importFrom rlang caller_env fn_env
#' @importFrom utils URLencode
#' @importFrom cli cli_abort cli_inform cli_warn
#' 
#' @keywords internal
#' @noRd
.cli_action <- function(msg,
                        ...,
                        action,
                        url = NULL,
                        hyperlink = NULL,
                        call = NULL) {

  caller_env <- rlang::caller_env()

  formatted_msg <- purrr::map2(.x = msg,
              .y = action,
              .f = function(m, a) {
                cli_sym <- switch(EXPR = a,
                                  "abort" = "x",
                                  "inform" = "i",
                                  "warn" = "!")
                cli_msg <- c(stats::setNames(object = m, nm = cli_sym))
              })
  
  formatted_msg <- unlist(x = formatted_msg)

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

  if (any(is.element(el = action, set = "abort"))) {
    cli::cli_abort(message = formatted_msg, ..., call = call, .envir = cli_env)
  } else if (any(is.element(el = action, set = "warn"))) {
    cli::cli_warn(message = formatted_msg, ..., call = call, .envir = cli_env)
  } else if (any(is.element(el = action, set = "inform"))) {
    cli::cli_inform(message = formatted_msg, ..., call = call, .envir = cli_env)
  } else {
    stop("invalid .cli_action action")
  }
}
