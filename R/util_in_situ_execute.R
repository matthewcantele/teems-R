#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
.execute_in_situ <- function(...,
                             cmf_path,
                             tab_file,
                             shock_file,
                             closure_file,
                             in_situ_writeout,
                             call,
                             quiet) {
  if (!missing(x = ...)) {
    io_files <- list(...)

    tab_file <- .check_input(
      file = tab_file,
      valid_ext = "tab",
      call = call,
      internal = FALSE
    )

    shock_file <- .check_input(
      file = shock_file,
      valid_ext = "shf",
      call = call,
      internal = FALSE
    )

    closure_file <- .check_input(
      file = closure_file,
      valid_ext = "cls",
      call = call,
      internal = FALSE
    )

    if (!quiet) {
      .cli_action(
        action = "inform",
        msg = "\"solve-in-situ\" mode activated."
      )
    }

    parsed_tablo <- .parse_tablo(tab_file = tab_file)
    tablo_files <- purrr::pluck(
      .x = .tablo_files(parsed_tablo = parsed_tablo[["extract"]]),
      "names"
    )
    req_files <- tablo_files[!is.element(el = tablo_files, set = "(new)")]

    if (!all(is.element(el = names(x = io_files), set = req_files))) {
      missing_files <- req_files[!is.element(
        el = names(x = io_files),
        set = req_files
      )]
      .cli_action(
        action = "abort",
        msg = "The Tablo file provided indicates that the following
                  files are required: {.val {req_files}} however one or more appear to
                  not have been provided: {.val {missing_files}}.",
        call = call
      )
    }

    if (!all(sapply(X = io_files, FUN = file.exists))) {
      missing_files <- names(x = io_files)[!sapply(
        X = io_files,
        FUN = file.exists
      )]
      .cli_action(
        action = "abort",
        msg = "One or more input files provided does not exist:
                  {.val {missing_files}}."
      )
    }

    io_files <- sapply(X = io_files, FUN = path.expand)
    model_dir <- dirname(path = tab_file)
    launchpad_dir <- file.path(model_dir, "launchpad")
    if (dir.exists(paths = launchpad_dir)) {
      unlink(x = launchpad_dir, recursive = TRUE, force = TRUE)
    }
    dir.create(path = file.path(launchpad_dir, "out", "sets"),
               recursive = TRUE)
    dir.create(path = file.path(launchpad_dir, "out", "coefficients"),
               recursive = TRUE)
    dir.create(path = file.path(launchpad_dir, "out", "variables", "bin"),
               recursive = TRUE)

    if (in_situ_writeout) {
      coeff_extract <- .tablo_coeff(parsed_tablo = parsed_tablo[["extract"]])
      coefficient_names <- coeff_extract[["name"]]
      tablo_sets <- .tablo_sets(parsed_tablo = parsed_tablo[["extract"]])[["sets"]]
      set_names <- toupper(x = tablo_sets[["name"]])
      parsed_tablo[["tab"]] <- .append_tablo(
        tab = parsed_tablo[["tab"]],
        coeff_extract = coeff_extract,
        sets = tablo_sets
      )
      tab_file <- file.path(launchpad_dir, parsed_tablo[["tab_file"]])
      writeLines(
        text = parsed_tablo[["tab"]],
        con = tab_file
      )
    }

    all_files <- c(io_files, shock_file = shock_file, closure_file = closure_file)

    for (f in seq_along(all_files)) {
      input_path <- all_files[f]
      file_name <- basename(path = input_path)
      new_path <- file.path(launchpad_dir, file_name)
      file.copy(from = input_path, to = new_path)
      all_files[f] <- new_path
    }

    io_files <- all_files[is.element(
      el = names(x = all_files),
      set = names(x = io_files)
    )]

    cmf_components <- paste(
      c("tabfile", "closure", "shock"),
      paste0(
        '"',
        c(tab_file, all_files[["closure_file"]], all_files[["shock_file"]]),
        '";'
      )
    )

    cmf_path <- .write_cmf(
      set_names = set_names,
      coefficient_names = coefficient_names,
      io_files = io_files,
      cmf_components = cmf_components,
      model_dir = model_dir,
      launchpad_dir = launchpad_dir,
      in_situ_writeout = in_situ_writeout,
      in_situ = TRUE
    )[["cmf_path"]]
  }

  return(cmf_path)
}