#' @importFrom data.table fread
#' 
#' @keywords internal
#' @noRd
.get_timesteps <- function(paths,
                           cmf_path,
                           timestep_header) {
  t0 <- readRDS(paths$metadata)$reference_year
  timestep_file <- .get_output_paths(
    cmf_path = cmf_path,
    which = "coefficient",
    select = .o_timestep_header()
  )
  timesteps <- data.table::fread(timestep_file, skip = 1, col.names = timestep_header)
  timesteps <- timesteps[!is.na(get(timestep_header))]
  timesteps[, CYRS := t0 + timesteps[, 1]]
  timesteps[, all_time := seq(0, nrow(timesteps) - 1)]
  return(timesteps)
}