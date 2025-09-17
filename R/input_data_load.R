#' @importFrom purrr map
#' 
#' @noRd
#' @keywords internal
.load_input_data <- function(dat_input,
                             par_input,
                             set_input,
                             aux_input,
                             call) {

  dat <- .read_har(con = dat_input,
                   attach_metadata = TRUE)
  metadata <- attr(dat, "metadata")
  par <- .read_har(con = par_input,
                   data_format = metadata$data_format)
  set <- .read_har(con = set_input,
                   data_format = metadata$data_format)
  
  if (!inherits(dat, "dat")) {
    inferred_type <- attr(dat, "data_type")
    .cli_action(data_err$invalid_dat_har,
                action = "abort",
                call = call
    )
  }
  
  if (!inherits(par, "par")) {
    inferred_type <- attr(par, "data_type")
    .cli_action(data_err$invalid_par_har,
                action = "abort",
                call = call
    )
  }
  
  if (!inherits(set, "set")) {
    inferred_type <- attr(set, "data_type")
    .cli_action(dat_err$invalid_set_har,
                action = "abort",
                call = call
    )
  }
  
  i_data <- c(dat, par, set)
  
  if (!is.null(aux_input)) {
    aux <- .read_har(con = aux_input)
    i_data <- c(i_data, aux)
  }

  i_data <- i_data[!names(i_data) %in% .o_full_exclude()]

  i_data <- lapply(
    i_data,
    function(h) {
      if (inherits(h, c("dat", "par")) && !is.null(dimnames(h))) {
        dimnames(h) <- purrr::map(dimnames(h), function(e) {
          tolower(gsub("CGDS", "zcgds", e, ignore.case = TRUE))
        })
      } else if (inherits(h, "set")) {
        h <- tolower(gsub(
          "CGDS",
          "zcgds",
          h,
          ignore.case = TRUE
        ))
      }
      return(h)
    }
  )

  .check_database_version(
    vetted = c("v9A", "v10A", "v11c"),
    provided = metadata$full_database_version,
    call = call
  )
  
  if (.o_verbose()) {
  .inform_metadata(metadata = metadata)
  }
  
  attr(i_data, "metadata") <- metadata
  class(i_data) <- c(metadata$data_format, class(i_data))
  return(i_data)
}