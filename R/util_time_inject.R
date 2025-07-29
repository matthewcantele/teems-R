#' @importFrom purrr map_chr map
#' 
#' @keywords internal
#' @noRd
.inject_time <- function(ls_data,
                         time_steps,
                         tab_file,
                         set_extract,
                         coeff_extract,
                         metadata,
                         call) {

  int_sets <- subset(
    set_extract,
    intertemporal
  )

  n_timestep <- length(time_steps)
  c_pattern <- paste0("[A-Za-z]{", 4, ",}")
  n_timestep_coeff <- unique(purrr::discard(
    purrr::map_chr(int_sets$definition, function(d) {
      if (nchar(d) > 4) {
        regmatches(d, gregexpr(c_pattern, d))[[1]]
      } else {
        NA
      }
    }), is.na
  ))

  int_sets$elements <- purrr::map(int_sets$definition, function(d) {
    .parse_tab_int(
      expr = d,
      n_timestep = n_timestep,
      n_timestep_coeff = n_timestep_coeff
    )
  })

  names(int_sets$elements) <- int_sets$name
  names(coeff_extract$ls_upper_idx) <- coeff_extract$header
  ls_data$dat <- lapply(ls_data$dat,
    .int_array,
    coeff_extract = coeff_extract,
    int_sets = int_sets
  )

  ls_data$par <- lapply(ls_data$par,
    .int_array,
    coeff_extract = coeff_extract,
    int_sets = int_sets
  )

  timestep_header <- .o_timestep_header()
  timestep_header_set <- purrr::pluck(coeff_extract, "ls_upper_idx", .o_timestep_header())
  timestep_header <- list(
    header = timestep_header,
    data = array(time_steps,
      dimnames = subset(int_sets, name %in% timestep_header_set, elements)[[1]]
    )
  )
  names(dimnames(timestep_header$data)) <- timestep_header_set

  n_timestep_header <- .o_n_timestep_header()
  n_timestep_header <- list(
    header = n_timestep_header,
    data = matrix(length(time_steps))
  )

  timestep_header <- list(timestep_header)
  names(timestep_header) <- .o_timestep_header()
  n_timestep_header <- list(n_timestep_header)
  names(n_timestep_header) <- .o_n_timestep_header()
  
  int_sets <- purrr::map2(
    int_sets$name,
    int_sets$elements,
    function(n, e) {
      list(
        header = n,
        data = e
      )
    }
  )

  names(int_sets) <- purrr::map_chr(int_sets, "header")
  ls_data$dat <- c(ls_data$dat, n_timestep_header)
  ls_data$par <- c(ls_data$par, timestep_header)
  ls_data$set <- c(ls_data$set, int_sets)
  
  return(ls_data)
}