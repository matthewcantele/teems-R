#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
.execute_in_situ <- function(input_files,
                             n_timesteps,
                             tab_file,
                             shock_file,
                             closure_file,
                             writeout,
                             call) {

  if (.o_verbose()) {
    .cli_action(
      solve_info$in_situ,
      action = "inform",
    )
  }
  
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

    browser()

    tab_comp <- .process_tablo(tab_file = tab_file)
    req_files <- setdiff(purrr::pluck(tab_comp, "file_extract", "names"),
                         "(new)")

    if (!all(req_files %in% names(input_files))) {
      missing_files <- setdiff(req_files, names(input_files))
      .cli_action(solve_err$insitu_missing_input,
        action = "abort",
        call = call
      )
    }

    if (!all(purrr::map_lgl(input_files, file.exists))) {
      nonexist_files <- input_files[!purrr::map_lgl(input_files, file.exists)]
      .cli_action(solve_err$insitu_no_file,
        action = "abort",
      )
    }

    input_files <- purrr::map_chr(input_files, normalizePath)
    model_dir <- dirname(tab_file)
    launchpad_dir <- file.path(model_dir, "launchpad")
    if (dir.exists(launchpad_dir)) {
      unlink(x = launchpad_dir, recursive = TRUE, force = TRUE)
    }
    dir.create(file.path(launchpad_dir, "out", "sets"),
               recursive = TRUE)
    dir.create(file.path(launchpad_dir, "out", "coefficients"),
               recursive = TRUE)
    dir.create(file.path(launchpad_dir, "out", "variables", "bin"),
               recursive = TRUE)

    browser()
    if (writeout) {
      coeff_extract <- purrr::pluck(parsed_tablo, "coeff_extract")
      tablo_sets <- purrr::pluck(parsed_tablo, "set_extract", "sets")
      set_names <- toupper(x = tablo_sets[["name"]])
      final.tablo <- .append_tablo(
        tab = tab_comp$conden_tab,
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

  return(cmf_path)
}