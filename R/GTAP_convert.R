#' @importFrom rlang arg_match
#' @importFrom purrr map2
#' 
#' @export
GTAP_convert <- function(...,
                         tab_file = NULL,
                         target_format = NULL,
                         data_type = NULL,
                         quiet = FALSE) {
  call <- match.call()
  if (!is.null(x = target_format)) {
    target_format <- rlang::arg_match(
      arg = target_format,
      values = c("v6.2", "v7.0")
    )
    if (is.null(x = tab_file)) {
      .cli_action(
        msg = "If {.arg target_format} is provided, a Tablo file
                  of the desired target format must be provided.",
        action = "abort"
      )
    }
  }
  src_har_file <- list(...)
  ls_array <- lapply(
    X = src_har_file,
    FUN = function(har) {
      read_har <- .read_har(
        con = har,
        data_type = NULL,
        call = call
      )
      attr(x = read_har, which = "metadata") <- .get_metadata(con = har)
      return(read_har)
    }
  )

  implied_data_types <- sapply(
    X = ls_array,
    FUN = attr,
    which = "data_type"
  )

  if (!quiet) {
    .cli_action(
      msg = c(
        "Header array files corresponding to the following
  {length(x = implied_data_types)} type{?s} have been provided:
                      {.val {implied_data_types}}.",
        "Note that if the detected type is incorrect, it can
                      be explicitly assigned via {.arg data_type}"
      ),
      action = c("inform", "inform")
    )
  }

  if (!is.null(x = data_type)) {
    if (!identical(x = length(x = src_har_file), y = length(x = data_type))) {
      .cli_action(
        msg = "The length of files provided and {.arg data_type}
                  must be the same.",
        action = "abort"
      )
    }
    if (!all(unlist(x = purrr::map2(
      .x = implied_data_types,
      .y = data_type,
      .f = identical
    )))) {
      id <- which(!unlist(x = purrr::map2(
        .x = implied_data_types,
        .y = data_type,
        .f = identical
      )))
      provided_type <- data_type[id]
      expected_type <- implied_data_types[id]
      .cli_action(
        msg = "Header array file(s) of explicitly provided type
     {.val {provided_type}} appear to be of type {.val {expected_type}}.",
        action = "warn"
      )
    }
  } else {
    data_type <- implied_data_types
  }

  if (!is.null(x = target_format)) {
    ls_array <- purrr::map2(
      .x = ls_array,
      .y = data_type,
      .f = function(lst, type) {
        tab_file <- .check_input(
          file = tab_file,
          valid_ext = "tab",
          call = call
        )
        if (identical(x = type, y = "set")) {
          set_extract <- .process_tablo(
            tab_file = tab_file,
            type = "set",
            call = call
          )
          cvrt_data <- .convert_set_format(
            set_array = lst,
            set_extract = set_extract[["sets"]],
            target_format = target_format
          )
        } else if (is.element(el = type, set = c("dat", "par"))) {
          coeff_extract <- .process_tablo(
            tab_file = tab_file,
            type = "coefficient",
            call = call
          )
          cvrt_data <- .convert_coeff_format(
            ls_array = lst,
            coeff_extract = coeff_extract,
            target_format = target_format,
            data_type = type
          )
        }

        attr(x = cvrt_data, which = "metadata") <- attr(x = lst, which = "metadata")
        attr(x = cvrt_data, which = "metadata")[["data_format"]] <- target_format
        return(cvrt_data)
      }
    )
  }
  
  if (length(ls_array) %=% 1L) {
    metadata <- attr(ls_array[[1]], "metadata")
    ls_array <- unlist(ls_array, recursive = FALSE)
    attr(ls_array, "metadata") <- metadata
  } else {
    names(x = ls_array) <- data_type
  }
  
  return(ls_array)
}