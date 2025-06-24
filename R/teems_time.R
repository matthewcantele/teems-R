#' @importFrom rlang arg_match
#'
#' @export
teems_time <- function(tab_file,
                       set_input = NULL,
                       time_steps,
                       time_format,
                       timestep_header = "YEAR",
                       n_timestep_header = "NTSP",
                       ...,
                       quiet = FALSE) {

  call <- match.call()
  tab_file <- .check_input(
    file = tab_file,
    valid_ext = "tab",
    call = call
  )
  
  if (!is.null(x = set_input)) {
    set_file <- .check_input(
      file = set_input,
      valid_ext = c("har", "qs2"),
      cache = FALSE,
      call = call
    )
  
    set_file_type <- attr(x = set_file, which = "file_ext")

    if (identical(x = set_file_type, y = "har")) {
    sets <- .read_har(
      con = set_input,
      data_type = "set",
      call = call
    )
    metadata <- .get_metadata(con = set_file)
    } else {
      metadata <- attr(x = set_file, which = "metadata")
      sets <- set_file
    }
  }

  
  # intertemporal set condition
  time_format <- rlang::arg_match(
    arg = time_format,
    values = c("chronological", "interval", "diff")
  )
  time_steps <- .check_time_steps(
    t0 = metadata[["reference_year"]],
    time_steps = time_steps,
    time_format = time_format,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    quiet = quiet,
    call = call
  )

  set_extract <- .process_tablo(
    type = "set",
    tab_file = tab_file
  )[["sets"]]

  coeff_extract <- .process_tablo(
    type = "coefficient",
    tab_file = tab_file
  )
  
  time_comp <- c(time_steps, list(...))
  aux_input <- purrr::map2(
    .x = time_comp,
    .y = names(x = time_comp),
    .f = function(comp, nme) {
      r_idx <- match(x = nme, table = coeff_extract[["header"]])
      coeff_sets <- coeff_extract[["ls_upper_idx"]][[r_idx]]
      if (any(!is.element(el = coeff_sets, set = c("ALLTIME", "null_set")))) {
        retrieve_sets <- coeff_sets[!is.element(el = coeff_sets, set = c("ALLTIME", "null_set"))]
        s_idx <- match(x = retrieve_sets, table = set_extract[["name"]])
        retrieve_headers <- set_extract[["header"]][s_idx]

        set_ele <- lapply(
          X = retrieve_headers,
          FUN = function(h) {
            purrr::pluck(.x = sets, h, "data")
          }
        )
        names(x = set_ele) <- retrieve_sets
        if (length(set_ele) > 1) {
          stop("fix here")
        }
        data <- array(data = rep(comp, length(set_ele[[1]])), dimnames = set_ele)
      } else {
        data <- comp
      }

      load_comp <- list(
        header = nme,
        label = coeff_extract[["label"]][r_idx],
        coefficient = coeff_extract[["coefficient"]][r_idx],
        data = data
      )
      return(load_comp)
    }
  )

  attr(x = aux_input, which = "n_timestep_header") <- n_timestep_header
  attr(x = aux_input, which = "timestep_header") <- timestep_header
  attr(x = aux_input, which = "metadata") <- metadata
  names(x = aux_input) <- sapply(X = aux_input, FUN = function(c){c[["header"]]})
  return(aux_input)
}
