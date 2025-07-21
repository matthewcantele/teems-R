#' @importFrom stats setNames
#' @importFrom rlang caller_env fn_env trace_back
#' @importFrom purrr map2
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

  formatted_msg <- purrr::map2(
    msg,
    action,
    function(m, a) {
      cli_sym <- switch(a,
        "abort" = "x",
        "inform" = "i",
        "warn" = "!"
      )
      cli_msg <- c(stats::setNames(m, cli_sym))
    }
  )

  formatted_msg <- unlist(formatted_msg)

  if (!is.null(url)) {
    cli_env <- new.env(parent = caller_env)
    url <- utils::URLencode(URL = url)
    if (!is.null(hyperlink)) {
      url_msg <- c("i" = "For additional information see: {.href [{hyperlink}]({url})}")
      cli_env$hyperlink <- hyperlink
    } else {
      url_msg <- c("i" = "For additional information see: {.href {url}}")
    }
    formatted_msg <- c(formatted_msg, url_msg)
    cli_env$url <- url
  } else {
    cli_env <- caller_env
  }

  if (is.null(call)) {
    call <- caller_env
  }

  if (any(action %in% "abort")) {
    cli::cli_abort(message = formatted_msg, ..., call = call, .envir = cli_env)
  } else if (any(action %in% "warn")) {
    cli::cli_warn(message = formatted_msg, ..., call = call, .envir = cli_env)
  } else if (any(action %in% "inform")) {
    cli::cli_inform(message = formatted_msg, ..., call = call, .envir = cli_env)
  } else {
    stop("invalid .cli_action action")
  }
}

.cli_missing <- function(arg) {
  if (!arg %=% "...") {
    arg <- deparse(substitute(arg))
  }
  call <- rlang::trace_back()$call[[1]]
  .cli_action("argument {.arg {arg}} is missing, with no default",
    action = "abort",
    call = call
  )
}