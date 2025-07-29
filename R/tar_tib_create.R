#' @importFrom purrr pmap_chr map_int
#'
#' @keywords internal
#' @noRd
.create_tib <- function(ls_,
                        unaggregated_input,
                        aggregated_input,
                        data_format,
                        sets,
                        coeff_extract) {

  tib_data <- .build_tibble(
    ls_ = ls_,
    sets = sets,
    unaggregated_input = unaggregated_input,
    coeff_extract = coeff_extract
  )


  tib_data <- .weight_param(
    tib_data = tib_data,
    data_format = data_format,
    sets = sets
  )

  tib_data <- .aggregate_data(
    tib_data = tib_data,
    sets = sets,
    coeff_extract = coeff_extract
  )


  if (!is.null(aggregated_input)) {
    tib_data <- .inject_agg_input(
      tib_data = tib_data,
      aggregated_input = aggregated_input
    )
  }

  tib_data$lead <- purrr::pmap_chr(
    .l = list(
      tib_data$ls_upper_idx,
      tib_data$type,
      tib_data$label,
      tib_data$header
    ),
    .f = function(upper, type, label, header) {
      if (!upper %=% "null_set") {
        set_dim <- with(
          sets$mapped_ele,
          lengths(mget(upper))
        )
        set_dim <- paste(set_dim, collapse = " ")
      } else {
        set_dim <- 1
      }
      lead <- paste(
        set_dim,
        type,
        "spreadsheet header",
        paste0('"', header, '"'),
        "longname",
        paste0('"', label, '";')
      )
    }
  )

  tib_data$idx <- purrr::map_int(tib_data$dt, .get_index)

  tib_data <- tib_data[order(tib_data$header), ]
  return(tib_data)
}