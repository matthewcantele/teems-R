#' @importFrom targets tar_read
#' @importFrom cli cli_fmt cli_h1 cli_h2 cli_h3 cli_dl cli_text cli_ul
#'   cli_verbatim cli_alert_warning
#' @importFrom purrr pmap map_chr
#'
#' @keywords internal
#' @noRd
.pipeline_diagnostics <- function(tab_path,
                                  model_dir,
                                  launchpad_dir,
                                  model_name,
                                  store_dir,
                                  pipeline_file,
                                  io_files,
                                  metadata,
                                  quiet) {
  
  mod_tab_path <- tar_read(write.tablo_mod, store = store_dir)
  orig_tab_path <- tar_read(write.tablo_orig, store = store_dir)
  sets <- targets::tar_read(final.set_tib, store = store_dir)
  closure <- targets::tar_read(final.closure, store = store_dir)
  closure <- head(tail(closure, -1), -3)
  shocks <- targets::tar_read(raw.shocks, store = store_dir)
  if (!is.null(shocks)) {
    shocks <- purrr::map_chr(shocks, "var")
    shock_var <- paste0(unique(shocks), collapse = ", ")
  } else {
    shocks <- NULL
    shock_var <- NULL
    if (!quiet) {
      null_shk <- cli::cli_fmt({
        cli::cli_alert_warning("No shock has been provided so a
      {.val NULL} shock will be used. A null shock will return all model
      coefficients as they are read and/or calculated in the Tablo file.
      Any significant deviation under these conditions would indicate an error
      in the loading of input files or parsing of model outputs.",
          wrap = TRUE
        )
      })
    }
  }

  if (any(sets$intertemporal)) {
    temporal_dynamics <- "intertemporal"
  } else {
    temporal_dynamics <- "static"
  }

  diagnostic_file <- file.path(launchpad_dir, "model_diagnostics.txt")
  if (file.exists(diagnostic_file)) {
    unlink(diagnostic_file)
  }

  diag_output <- cli::cli_fmt({
    # Start output generation
    cli::cli_h1("Diagnostic outputs follow")
    cli::cli_h2("General model specifications")
    cli::cli_dl(c(
      "Modeled Tablo file" = mod_tab_path,
      "Original Tablo file" = orig_tab_path,
      "Temporal dynamics" = temporal_dynamics,
      "GTAP database version" = metadata$orig_database_version,
      "Reference year" = metadata$reference_year,
      "Data format" = metadata$data_format
    ))
    cli::cli_h2(text = "Set specifications")
    purrr::pmap(
      .l = list(sets$name, sets$mapped_ele, sets$label),
      .f = function(nme, ele, info) {
        nme <- toupper(nme)
        info <- trimws(gsub("#", "", info))
        cli::cli_h3(text = "Set {nme}")
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
    if (exists("null_shk")) {
      cli::cli_verbatim(diag_output, null_shk)
    } else {
      cli::cli_verbatim(diag_output)
    }
  }
  writeLines(diag_output,
             file.path(launchpad_dir, "model_diagnostics.txt"))
}
