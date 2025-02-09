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
                                  metadata,
                                  quiet) {
  tab_path <- file.path(launchpad_dir,
                        targets::tar_read(name = parsed.tablo, store = store_dir)[["tab_file"]])
  sets <- targets::tar_read(name = final.set_tib, store = store_dir)
  dat <- targets::tar_read(name = final.base_tib, store = store_dir)
  param <- targets::tar_read(name = final.par_tib, store = store_dir)
  closure <- tar_read(name = final.closure, store = store_dir)
  closure <- head(tail(closure, -1), -3)
  shocks <- targets::tar_read(name = constructed.shocks, store = store_dir)[[1]]
  shock_var <- paste0(unique(x = substring(text = names(x = shocks),
                                    first = 1,
                                    last = nchar(x = names(x = shocks)) - 1)), collapse = ", ")

  if (any(sets[["intertemporal"]])) {
    temporal_dynamics <- "intertemporal"
  } else {
    temporal_dynamics <- "static"
  }

diagnostic_file <- file.path(launchpad_dir, "model_diagnostics.txt")
if (file.exists(diagnostic_file)) {
unlink(x = diagnostic_file)
}
r_idx <- match(x = dat[["input_file"]], table = names(x = io_files))
dat[["file_path"]] <- io_files[r_idx]

diag_output <- cli::cli_fmt({
# Start output generation
cli::cli_h1("Diagnostic outputs follow")
cli::cli_h2("General model specifications")
cli::cli_dl(c(
  "Tablo file" = tab_path,
  "Temporal dynamics" = temporal_dynamics,
  "GTAP database version" = metadata[["orig_database_version"]],
  "Reference year" = metadata[["reference_year"]],
  "Data format" = metadata[["data_format"]]
))
cli::cli_h2("Set specifications")
purrr::pmap(
  .l = list(sets[["name"]], sets[["elements"]], sets[["information"]]),
  .f = function(nme, ele, info) {
    nme <- toupper(x = nme)
    info <- trimws(x = gsub(pattern = "#", replacement = "", x = info))
    cli::cli_h3("Set {nme}")
    cli::cli_text("Description: {info}")
    cli::cli_text("Elements: {.val {paste(ele, collapse = ', ')}}")
  }
)
cli::cli_h2("Closure and shock specifications")
cli::cli_dl(c(
  "Exogenous variables" = toString(closure),
  "Number of shocks" = length(shocks),
  "Variables shocked" = shock_var
))
cli::cli_h2("File and directory related")
cli::cli_dl(c(
  "Model pipeline" = file.path(model_dir, model_name),
  "Model {{targets}} store written to" = store_dir
))
cli::cli_text("Input files written to:")
cli::cli_ul(paste(names(io_files), io_files, sep = ": "))
})
if (!quiet) {
  cli::cli_verbatim(diag_output)
}
writeLines(text = diag_output,
           con = file.path(launchpad_dir, "model_diagnostics.txt"))
}
