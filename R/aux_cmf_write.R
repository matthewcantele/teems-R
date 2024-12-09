#' @importFrom targets tar_read
#' @importFrom purrr compact
.cmf_write <- function(tab_file = NULL,
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
    io_files <- list(
      GTAPDATA = targets::tar_read(write.dat, store = store_dir),
      GTAPPARM = targets::tar_read(write.par, store = store_dir),
      GTAPSETS = targets::tar_read(write.sets, store = store_dir),
      INTPARM = targets::tar_read(write.int_par, store = store_dir),
      INTDATA = targets::tar_read(write.int_dat, store = store_dir)
    )

    # remove int if static
    io_files <- purrr::compact(.x = io_files)
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
    cmf_components <- paste(c("tabfile", "closure", "shock"), paste0(
      '"',
      c(
        targets::tar_read(write.tablo, store = store_dir),
        targets::tar_read(write.closure, store = store_dir),
        targets::tar_read(write.shocks, store = store_dir)
      ),
      '";'
    ))
  } else {
    cmf_components <- paste(c("tabfile", "closure", "shock"), paste0('"', c(tab_file, closure_file, shock_file), '";'))
  }

  if (!in_situ) {
    coefficient_names <- targets::tar_read(name = tablo_coeff, store = store_dir)[["name"]]
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

  .TEEMS_write(
    input = cmf,
    file = cmf_file,
    write_object = "cmf",
    write_dir = launchpad_dir
  )

  # full cmf path
  cmf_path <- file.path(launchpad_dir, cmf_file)

  gen_out <- list(cmf_path = cmf_path, io_files = io_files)

  return(gen_out)
}
