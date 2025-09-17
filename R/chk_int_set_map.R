#' @keywords internal
#' @noRd
.check_internal_set_map <- function(map_name,
                                    set_map,
                                    available_mappings,
                                    call) {

  if (!map_name %in% names(available_mappings)) {
    .cli_action(
      load_err$no_internal_mapping,
      action = "abort",
      call = call
    )
  }

  available_map_names <- names(available_mappings[[map_name]])[-1]
  if (!set_map %in% available_map_names) {
    .cli_action(
      load_err$invalid_internal_mapping,
      action = c("abort", "inform"),
      call = call
    )
  }

  available_mappings <- available_mappings[[map_name]]
  sel_mapping <- available_mappings[, c(1, which(names(available_mappings) == set_map)), with = FALSE]
  return(sel_mapping)
}