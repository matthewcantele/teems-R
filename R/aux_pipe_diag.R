#' @importFrom targets tar_read
#' @importFrom purrr map2
#' @keywords internal
#' @noRd
.pipeline_diagnostics <- function(model_dir,
                                  launchpad_dir,
                                  model_name,
                                  store_dir,
                                  pipeline_file,
                                  io_files,
                                  verbose) {

  tab_path <- file.path(launchpad_dir,
                        targets::tar_read(name = parsed.tablo, store = store_dir)[["tab_file"]])
  sets <- targets::tar_read(name = final.set_tib, store = store_dir)
  coeff <- targets::tar_read(name = tablo_coeff, store = store_dir)

  dat <- subset(x = coeff,
                  subset = {is.element(el = file, set = "GTAPDATA")})

  param <- subset(x = coeff,
                  subset = {is.element(el = file, set = "GTAPPARM")})

  metadata <- targets::tar_read(name = metadata, store = store_dir)
  closure <- tar_read(name = final.closure, store = store_dir)
  closure <- head(tail(closure, -1), -3)
  shocks <- targets::tar_read(name = constructed.shocks, store = store_dir)[[1]]
  shock_var <- paste0(unique(x = substring(text = names(x = shocks),
                                    first = 1,
                                    last = nchar(x = names(x = shocks)) - 1)), collapse = ", ")

  core_files <- targets::tar_read(name = tab_file_check, store = store_dir)

  if (is.element(el = "INTDATA", set = names(x = io_files))) {
    temporal_dynamics <- "intertemporal"
  } else {
    temporal_dynamics <- "static"
  }

diagnostic_file <- file.path(launchpad_dir, "model_diagnostics.txt")
if (file.exists(diagnostic_file)) {
unlink(x = diagnostic_file)
}

# Start output generation
.diagnostic_message(message_text = "#### Diagnostic outputs follow ####",
              verbose = verbose,
              write_path = diagnostic_file)

.diagnostic_message(message_text = paste("## General model specifications ##",
                    "\n",
                    "Tablo file:", tab_path,
                    "\n",
                    "Temporal dynamics:", temporal_dynamics,
                    "\n",
                    "GTAP database version:", metadata[["orig_database_version"]],
                    "\n",
                    "Reference year:", metadata[["reference_year"]],
                    "\n",
                    "Data format:", metadata[["data_format"]],
                    "\n"),
              verbose = verbose,
              write_path = diagnostic_file)

.diagnostic_message(message_text = "## Set specifications ##",
                    verbose = verbose,
                    write_path = diagnostic_file)

purrr::pmap(.l = list(sets[["name"]],
                      sets[["elements"]],
                      sets[["information"]]),
            .f = function(nme, ele, info) {
              nme <- toupper(x = nme)
              info <- trimws(x = gsub(pattern = "#", replacement = "", x = info))
              .diagnostic_message(message_text = paste("Set", nme, paste0(dQuote(x = info), ":"), paste(ele, collapse = ", ")),
                                  verbose = verbose,
                                  write_path = diagnostic_file)
            })

.diagnostic_message(message_text = paste("\n## Data specifications ##",
                    "\nCoefficients written to:", io_files[["GTAPDATA"]]),
                    verbose = verbose,
                    write_path = diagnostic_file)

purrr::map2(.x = dat[["name"]],
            .y = dat[["information"]],
            .f = function(nme, info) {
              info <- trimws(x = gsub(pattern = "#", replacement = "", x = info))
              .diagnostic_message(message_text = paste(nme, dQuote(x = info)),
                                  verbose = verbose,
                                  write_path = diagnostic_file)
            })

.diagnostic_message(message_text = paste("\n## Parameter specifications ##",
                    "\nParameters written to:", io_files[["GTAPDATA"]]),
                    verbose = verbose,
                    write_path = diagnostic_file)

purrr::map2(.x = param[["name"]],
            .y = param[["information"]],
            .f = function(nme, info) {
              info <- trimws(x = gsub(pattern = "#", replacement = "", x = info))
              .diagnostic_message(message_text = paste(nme, dQuote(x = info)),
                                  verbose = verbose,
                                  write_path = diagnostic_file)
            })

.diagnostic_message(message_text = paste("\n## Closure and shock specifications ##",
                    "\nExogenous variables:", paste0(closure, collapse = "\n"),
                    "\nRest endogenous\n",
                    "\nNumber of shocks:", length(shocks),
                    "\nVariables shocked:", shock_var),
                    verbose = verbose,
                    write_path = diagnostic_file)

.diagnostic_message(message_text = "\n## File and directory related ##",
                    verbose = verbose,
                    write_path = diagnostic_file)

.diagnostic_message(message_text = paste("Model pipeline written to:", file.path(model_dir, model_name)),
                    verbose = verbose,
                    write_path = diagnostic_file)

.diagnostic_message(message_text = paste("Model `targets`", dQuote(x = "store"), "written to:", store_dir),
                    verbose = verbose,
                    write_path = diagnostic_file)

purrr::map2(.x = core_files,
            .y = names(x = core_files),
            .f = function(csv, nme) {
              .diagnostic_message(message_text = paste(nme, "written to:", file.path(launchpad_dir, csv)),
                                  verbose = verbose,
                                  write_path = diagnostic_file)
            })



}
