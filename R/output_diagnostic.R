#' @importFrom cli cli_fmt cli_h1 cli_h2 cli_h3 cli_dl cli_text cli_ul
#'   cli_verbatim cli_alert_warning
#' @importFrom purrr pmap map_chr
#'
#' @keywords internal
#' @noRd
.diagnostic_output <- function(tab_path,
                               cmf_path,
                               closure_path,
                               shock_path,
                               data_path,
                               sets,
                               closure,
                               shocks,
                               metadata) {

  if (!is.null(shocks)) {
    shocks <- purrr::map_chr(shocks, "var")
    shock_var <- paste0(unique(shocks), collapse = ", ")
  } else {
    shocks <- NULL
    shock_var <- NULL
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

  if (any(grepl("\\(intertemporal\\)", sets$qualifier_list))) {
    temporal_dynamics <- "intertemporal"
  } else {
    temporal_dynamics <- "static"
  }

  diagnostic_file <- file.path(dirname(tab_path), "model_diagnostics.txt")
  diag_output <- cli::cli_fmt({
    # Start output generation
    cli::cli_h1("Diagnostic outputs follow")
    cli::cli_h2("General model specifications")
    cli::cli_dl(c(
      "Modeled Tablo file" = tab_path,
      "Input files" = toString(data_path),
      "Temporal dynamics" = temporal_dynamics,
      "GTAP database version" = metadata$full_database_version,
      "Reference year" = metadata$reference_year,
      "Data format" = metadata$data_format
    ))
    cli::cli_h2(text = "Set specifications")
    purrr::pmap(
      .l = list(sets$name, sets$ele, sets$label),
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
      "Exogenous components" = toString(closure),
      "Number of shocks" = length(shocks),
      "Variables shocked" = shock_var
    ))
  })

  if (.o_verbose()) {
    if (exists("null_shk")) {
      cli::cli_verbatim(diag_output, null_shk)
    } else {
      cli::cli_verbatim(diag_output)
    }
  }

  .ems_write(metadata, write_dir = dirname(tab_path))
  writeLines(
    diag_output,
    diagnostic_file
  )
}