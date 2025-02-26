#' @importFrom targets tar_read
#' 
#' @keywords internal
#' @noRd
.write_cmf <- function(tab_file = NULL,
                       closure_file = NULL,
                       shock_file = NULL,
                       io_files = NULL,
                       model_name = "teems",
                       model_dir = NULL,
                       store_dir = NULL,
                       launchpad_dir = NULL,
                       in_situ = FALSE) {
  if (in_situ) {
    if (is.null(x = tab_file)) {
      stop(paste(
        "The 'tab_file' argument is required in order to",
        dQuote(x = "solve-in-situ")
      )) }

      if (!file.exists(tab_file)) {
        stop(paste(
          "The path to Tab file provided in",
          dQuote(x = "solve-in-situ"),
          "mode does not exist."
        ))
      }

    if (is.null(x = closure_file)) {
      stop(paste(
        "The 'closure_file' argument is required in order to",
        dQuote(x = "solve-in-situ")
      ))}

    if (!file.exists(closure_file)) {
        stop(paste(
          "The path toclosure file provided in",
          dQuote(x = "solve-in-situ"),
          "mode does not exist."
        ))
      }

    if (is.null(x = shock_file)) {
      stop(paste(
        "The 'shock_file' argument is required in order to",
        dQuote(x = "solve-in-situ")
      )) }

    if (!file.exists(shock_file)) {
        stop(paste(
          "The path to shock file provided in",
          dQuote(x = "solve-in-situ"),
          "mode does not exist."
        ))
      }

    message(dQuote(x = "solve-in-situ"), " mode activated")
  }

  if (!in_situ) {
    # io files
    io_files <- c(targets::tar_read(write.base, store = store_dir),
                  targets::tar_read(write.par, store = store_dir),
                  targets::tar_read(write.set, store = store_dir))
    
    io_names <- sapply(X = io_files,
                       FUN = function(f) {sub(pattern = ".txt",
                                              replacement = "",
                                              x = basename(path = f))})
    names(x = io_files) <- io_names
  } else {
    # launchpad_dir for user-provided same as tab file
    parsed.tablo <- .parse_tablo(tab_file = tab_file)
    tablo_files <- .tablo_files(parsed_tablo = parsed.tablo[["extract"]])

    io_files <- sapply(X = io_files, FUN = path.expand)
    tab_file <- path.expand(path = tab_file)
    closure_file <- path.expand(path = closure_file)
    shock_file <- path.expand(path = shock_file)

    # check that all io files have been introduced
    if (!all(is.element(el = tablo_files[["names"]], set = names(x = io_files)))) {
      missing_files <- tablo_files[["names"]][!is.element(el = tablo_files[["names"]], set = names(x = io_files))]
      stop(
        paste(
          "The following input files are required by the Tab file but not provided:",
          toString(x = missing_files)
        )
      )
    }

    if (!all(sapply(X = io_files, FUN = file.exists))) {
      stop("One or more input files provided does not exist.")
    }
  }

  if (!in_situ) {
    # tab, closure, and shock
    cmf_components <- c(targets::tar_read(write.tablo, store = store_dir),
                        targets::tar_read(write.closure, store = store_dir),
                        targets::tar_read(write.shocks, store = store_dir))
    
    cmf_components <- paste(names(cmf_components), paste0("\"", cmf_components, "\"", ";"))
  } else {
    cmf_components <- paste(c("tabfile", "closure", "shock"), paste0('"', c(tab_file, closure_file, shock_file), '";'))
  }

  if (!in_situ) {
    coefficient_names <- targets::tar_read(name = coeff_extract, store = store_dir)[["name"]]
  } else {
    coefficient_names <- .tablo_coeff(parsed_tablo = parsed.tablo[["extract"]])[["name"]]
    launchpad_dir <- dirname(path = tab_file)
    model_dir <- dirname(path = launchpad_dir)
    # check that all files are located in the same directory and copy
    # if not
    for (f in c(io_files, shock_file, closure_file)) {
      if (!identical(x = f, y = file.path(launchpad_dir, basename(path = f)))) {
        file.copy(from = f, to = launchpad_dir)
      }
    }
  }

  # coefficient writeout
  coefficient_writeout <- paste("outdata",
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
                                ))

  # write cmf
  iodata <- paste("iodata",
                  paste0('"', names(x = io_files), '"'),
                  paste0('"', io_files, '";'))

  # variable writeout
  variable_output <- paste("soldata", '"SolFiles"', paste0(
    '"',
    file.path(launchpad_dir, "out", "variables", "bin", "sol"),
    '";'
  ))

  cmf <- capture.output(cat(
    iodata,
    cmf_components,
    variable_output,
    coefficient_writeout,
    sep = "\n"
  ))

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
