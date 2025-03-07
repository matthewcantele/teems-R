#' @importFrom targets tar_read
#' 
#' @keywords internal
#' @noRd
.write_cmf <- function(model_name = "teems",
                       model_dir = NULL,
                       store_dir = NULL,
                       launchpad_dir = NULL,
                       set_names = NULL,
                       coefficient_names = NULL,
                       io_files = NULL,
                       cmf_components = NULL,
                       in_situ_writeout = TRUE,
                       in_situ = FALSE) {

  if (!in_situ) {
    # io files
    io_files <- c(
      targets::tar_read(write.base, store = store_dir),
      targets::tar_read(write.par, store = store_dir),
      targets::tar_read(write.set, store = store_dir)
    )

    io_names <- sapply(
      X = io_files,
      FUN = function(f) {
        sub(
          pattern = ".txt",
          replacement = "",
          x = basename(path = f)
        )
      }
    )
    names(x = io_files) <- io_names
    # tab, closure, and shock
    cmf_components <- c(
      targets::tar_read(write.tablo, store = store_dir),
      targets::tar_read(write.closure, store = store_dir),
      targets::tar_read(write.shocks, store = store_dir)
    )

    cmf_components <- paste(names(cmf_components), paste0("\"", cmf_components, "\"", ";"))
    coefficient_names <- targets::tar_read(name = coeff_extract, store = store_dir)[["name"]]
    set_names <- toupper(x = targets::tar_read(name = final.set_tib, store = store_dir)[["name"]])
  }

  if (in_situ_writeout) {
    # set writeout
    set_writeout <- paste(
      "outdata",
      paste0('"', set_names, '"'),
      paste0(
        '"',
        file.path(
          launchpad_dir,
          "out",
          "sets",
          paste0(set_names, ".csv")
        ),
        '"',
        ";"
      )
    )
    # coefficient writeout
    coefficient_writeout <- paste(
      "outdata",
      paste0('"', coefficient_names, '"'),
      paste0(
        '"',
        file.path(
          launchpad_dir,
          "out",
          "coefficients",
          paste0(coefficient_names, ".csv")
        ),
        '"',
        ";"
      )
    )
  }

  # write cmf
  iodata <- paste(
    "iodata",
    paste0('"', names(x = io_files), '"'),
    paste0('"', io_files, '";')
  )

  # variable writeout
  variable_output <- paste("soldata", '"SolFiles"', paste0(
    '"',
    file.path(launchpad_dir, "out", "variables", "bin", "sol"),
    '";'
  ))

  if (in_situ_writeout) {
    cmf <- capture.output(cat(
      iodata,
      cmf_components,
      variable_output,
      set_writeout,
      coefficient_writeout,
      sep = "\n"
    ))
  } else {
    cmf <- capture.output(cat(
      iodata,
      cmf_components,
      variable_output,
      sep = "\n"
    ))
  }

  cmf <- sapply(
    X = cmf,
    FUN = gsub,
    USE.NAMES = FALSE,
    pattern = model_dir,
    replacement = "/home"
  )

  cmf_file <- paste0(model_name, ".cmf")

  cmf_path <- .TEEMS_write(
    input = cmf,
    file = cmf_file,
    write_object = "cmf",
    write_dir = launchpad_dir
  )

  gen_out <- list(cmf_path = cmf_path, io_files = io_files)

  return(gen_out)
}
