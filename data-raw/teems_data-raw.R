library(targets)

targets::tar_config_set(store = "./data-raw/_targets")

# Set target options:
targets::tar_option_set(
  packages = c("data.table", "usethis", "purrr", "tabulapdf", "dplyr", "fs"),
  format = "qs",
  cue = tar_cue("always")
)



# functions
targets::tar_source("./data-raw/R")

list(
  tar_target(
    db_version,
    c("v9", "v10", "v11")
  ),
  tar_target(
    data_format,
    c("v6.2", "v7.0")
  ),
  # mapping related --------------------------------------------------
  tar_target(
    mapping_csvs,
    {
      fs::dir_copy(file.path("../teems-mappings/", db_version),
                   file.path("./data-raw/mappings", db_version),
                   overwrite = TRUE)

    },
    pattern = map(db_version)
  ),
  tar_target(
    mapping_files,
    {
      list.files(
        path = "./data-raw/mappings",
        recursive = TRUE,
        full.names = TRUE
      )
    }
  ),
  tar_target(
    mappings,
    process_mappings(
      path = mapping_files,
      db_version = db_version,
      data_format = data_format
    )
  ),
  # tab related ------------------------------------------------------
  tar_target(
    tab_repo,
    list.files(
      pattern = "\\.tab",
      path = file.path("../teems-tabs"),
      full.names = TRUE
    ),
    format = "file"
  ),
  tar_target(
    tab_files,
    {
      package_tabs <- file.path("data-raw", "tab_files")
      file.copy(
        from = tab_repo,
        to = package_tabs,
        overwrite = TRUE
      )
      list.files(
        path = package_tabs,
        full.names = TRUE
      )
    },
    format = "file"
  ),
  tar_target(
    tab_names,
    {
      lapply(tab_files, function(tab) {
        tools::file_path_sans_ext(x = basename(path = tab))
      })
    }
  ),
  tar_target(
    internal_tab,
    {
      tabs <- purrr::map2(
        tab_files,
        tab_names,
        function(tab, nme) {
          tab <- readChar(
            con = tab,
            nchars = file.info(tab)[["size"]]
          )
          class(tab) <- nme
          return(tab)
        }
      )

      names(x = tabs) <- tab_names
      return(tabs)
    }
  ),

  # parameter related ------------------------------------------------
  # static ===========================================================
  tar_target(
    v6.2_weights,
    {
      list(
        ESBD = c("VDPA", "VIPA", "VDGA", "VIGA", "VDFA", "VIFA"),
        ESBM = c("VIPA", "VIGA", "VIFA"),
        ESBT = c("VDFM", "VIFM", "VFM", "FTRV", "-FBEP", "-ISEP"),
        ESBV = "EVFA",
        INCP = c("VDPA", "VIPA"),
        SUBP = c("VDPA", "VIPA")
      )
    }
  ),
  tar_target(
    v7.0_weights,
    {
      list(
        ESBD = c("VDPP", "VMPP", "VMGP", "VDGP", "VDFP", "VMFP"),
        ESBM = c("VMPP", "VMGP", "VMFP"),
        ESBT = c("VDFB", "VMFB", "EVFB", "FTRV", "-FBEP", "-ISEP"),
        ESBV = "EVFP",
        INCP = c("VDPP", "VMPP"),
        SUBP = c("VDPP", "VMPP")
      )
    }
  ),
  tar_target(
    param_weights,
    {
      list(v6.2 = v6.2_weights, v7.0 = v7.0_weights)
    }
  ),

  # closures ---------------------------------------------------------
  tar_target(
    closure_repo,
    {
      list.files(
        path = file.path("../teems-closures/"),
        full.names = TRUE
      )
    }
  ),
  tar_target(
    closure_files,
    {
      package_closures <- file.path("data-raw", "closures")
      file.copy(
        from = closure_repo,
        to = package_closures,
        recursive = T
      )
      list.files(
        path = package_closures,
        recursive = TRUE,
        full.names = TRUE
      )
    },
    format = "file"
  ),
  tar_target(
    internal_cls,
    {
      cls <- lapply(
        X = closure_files,
        FUN = function(closure) {
          readLines(closure)
        }
      )

      cls_names <- lapply(closure_files, function(cls) {
        tools::file_path_sans_ext(x = basename(path = cls))
      })

      names(x = cls) <- cls_names
      return(cls)
    }
  ),
  # v6.2 <> v7 conversion
  tar_target(
    GTAPv7_manual,
    "./data-raw/aux/Corong and Tsigas - 2017 - The Standard GTAP Model, Version 7.pdf",
    format = "file"
  ),
  tar_target(
    set_table,
    {
      tibble::tibble(
        v6.2_upper = c("TRAD_COMM", "PROD_COMM", "ENDW_COMM", "MARG_COMM", "ALLTIME"),
        v6.2_mixed = c("TRAD_COMMi", "PROD_COMMj", "ENDW_COMMi", "MARG_COMMm", "ALLTIMEt"),
        v7.0_upper = c("COMM", "ACTS", "ENDW", "MARG", "ALLTIME"),
        v7.0_mixed = c("COMMc", "ACTSs", "ENDWe", "MARGm", "ALLTIMEt")
      )
    }
  ),

  # tables here are not even concordance, just semi related lists
  tar_target(
    set_conversion,
    {
      set_table <- tabulapdf::extract_tables(
        file = GTAPv7_manual,
        pages = 83
      )
      c_names <- c("name", "header", "description")

      v6.2_sets <- set_table[[1]][, c(1:4)]
      colnames(x = v6.2_sets) <- c("idx", paste0("v6.2", c_names))
      v7.0_sets <- set_table[[1]][, c(5:8)]
      colnames(x = v7.0_sets) <- c("idx", paste0("v7.0", c_names))

      # always inconsistencies in GTAP outputs
      v7.0_sets[5:13, "idx"] <- 5:13
      v7.0_sets[12:13, "idx"] <- 13:14

      sets <- merge(v6.2_sets,
        v7.0_sets,
        by = "idx",
        all = TRUE
      )
    }
  ),
  tar_target(
    param_conversion,
    {
      param_table <- tabulapdf::extract_tables(
        file = GTAPv7_manual,
        pages = 84
      )

      v7.0_param <- param_table[[1]][, c(5:8)]

      # missing ESBQ
      ESBQ <- tibble::tibble(14, "ESBQ", "COMM*REG", "1/CES elasticity for commodity sourcing")
      colnames(x = ESBQ) <- colnames(x = v7.0_param)

      # missing ESBI
      # ESBI <- tibble::tibble(15, "ESBI", "REG", "Investment expenditure CES elasticity")
      # colnames(x = ESBI) <- colnames(x = v7.0_param)

      v7.0_param <- rbind(v7.0_param, ESBQ)

      v7.0_param <- .table_fix(
        single = c(11, 12, 27),
        double = c(1, 3, 5, 7, 9, 13, 15, 17, 19, 25),
        trebble = c(19, 22),
        table = v7.0_param,
        prefix = "v7.0",
        data_type = "par"
      )

      v7.0_param[10:14, "idx"] <- 11:15

      v6.2_param <- param_table[[1]][, c(1:4)]

      v6.2_param <- .table_fix(
        single = c(11, 12),
        double = c(1, 3, 5, 7, 9, 13, 15, 17),
        table = v6.2_param,
        prefix = "v6.2",
        data_type = "par"
      )

      param <- merge(v6.2_param,
        v7.0_param,
        by = "idx",
        all = TRUE
      )
      param[["data_type"]] <- "par"
      return(param)
    }
  ),
  tar_target(
    dat_conversion,
    {
      coeff_table <- tabulapdf::extract_tables(
        file = GTAPv7_manual,
        pages = 85:86
      )

      coeff_table <- data.table::rbindlist(l = coeff_table)

      v7.0_coeff <- coeff_table[, c(4:6)]
      double <- c(3, 18, 20, 22, 24, 26, 30, 32, 35, 37, 41, 49, 51, 54)
      single <- setdiff(seq(1, nrow(v7.0_coeff)), double)
      NAs <- which(is.na(v7.0_coeff[, 1]))
      single <- setdiff(single, NAs)
      v7.0_coeff <- .table_fix(
        single = single,
        double = double,
        table = v7.0_coeff,
        prefix = "v7.0",
        data_type = "dat"
      )

      v6.2_coeff <- coeff_table[, c(1:3)]
      double <- c(18, 20, 22, 24, 26, 30, 32, 35, 37, 41)
      single <- setdiff(seq(1, nrow(v6.2_coeff)), double)

      NAs <- which(is.na(v6.2_coeff[, 1]))
      single <- setdiff(single, NAs)
      v6.2_coeff <- .table_fix(
        single = single,
        double = double,
        table = v6.2_coeff,
        prefix = "v6.2",
        data_type = "dat"
      )

      coeff <- merge(v6.2_coeff,
        v7.0_coeff,
        by = "idx",
        all = TRUE
      )
      coeff[["v6.2set"]] <- NA
      coeff[["v7.0set"]] <- NA
      coeff[["data_type"]] <- "dat"
      return(coeff)
    }
  ),

  tar_target(
    coeff_conversion,
    {
      rbind(dat_conversion, param_conversion)
    }
  ),

  # messages
  tar_target(
    gen_err,
    {
      list(class = "{.arg {arg_name}} must be a {.or {accepted_class}}, not {.obj_type_friendly {provided_class}}.",
           dir_not_file = "A filepath is expected, not the directory {.file {file}}.",
           no_file = "Cannot open file {.file {file}}: No such file.",
           invalid_file = "{.arg {arg}} must be a {.or {.val {valid_ext}}} file, not {?a/an} {.val {file_ext}} file.",
           invalid_internal = c("The specified internal {file_type} file: {.val {file}} is not supported.",
                                "Currently supported {file_type} files include: {.val {file_names}}.",
                                "Alternatively, path to a user-provided {file_type} file is supported (e.g., \"/my/{file_type}/path.{ext}\")",
                                "Note that user-provided {file_type} files may need to be modified for compatibility with various {.pkg teems} functions."))
    }
  ),

  tar_target(
    gen_wrn,
    {
      list(db_version = c("{.pkg teems} version: {teems_version} has only been vetted on GTAP Data Base versions: {vetted}.",
                          "The {.fn teems::emssolve} function can bypass the pipeline and be called on solver-ready input files."))
    }
  ),

  tar_target(
    gen_info,
    {
      list(dat = c("GTAP Data Base version: {.field {full_database_version}}",
                   "Reference year: {.field {reference_year}}",
                   "Data format: {.field {data_format}}"))
    }
  ),

  tar_target(
    gen_url,
    {
      list(internal_files = NULL)
    }
  ),

  tar_target(
    data_err,
    {
      list(missing_tar = "If {.arg target_format} is provided for conversion, a Tablo file of the desired target format must be provided.",
           invalid_convert = "The HAR file provided for {.arg dat_input} is already of the {.arg target_format} {.val {target_format}}.",
           missing_tab = "If {.arg time_steps} is provided for a dynamic model, a Tablo file must be provided.",
           invalid_dat_har = "The header array file provided for {.arg dat_input} appears to be of type {.val {inferred_type}}, not {.val dat}.",
           invalid_par_har = "The header array file provided for {.arg par_input} appears to be of type {.val {inferred_type}}, not {.val par}.",
           invalid_set_har = "The header array file provided for {.arg set_input} appears to be of type {.val {inferred_type}}, not {.val set}.",
           invalid_time_step = "One or more {.arg time_steps} does not progress into the future."
           )
    }
  ),

  tar_target(
    data_wrn,
    {
      list(time_steps = "The initial timestep provided is neither {.val 0} nor the reference year corresponding to the {.field dat} file loaded: {.val {t0}}.")
    }
  ),

  tar_target(
    load_err,
    {
      list(nested_class = "Input data must be provided as a {.or {data_class}}, not {.obj_type_friendly {errant_class}}.",
           invalid_input = "The input header provided {.field {nme}} is not among loaded data headers: {.field {existing_headers}}.",
           no_val_col = "Input data for the header {.field {nme}} does contain {.val Value} as the final column.",
           unagg_missing_tup = "{n_missing_tuples} tuple{?s} in the provided input file for {.val {nme}} were missing: {.field {missing_tuples}}.",
           unagg_missing_col = c("Input data for the header {.field {nme}} does not contain all required columns (sets).",
                                 "The required columns are {.field {req_col}}."),
           missing_eqm_input = "If {.arg eqm_input} is not provided, both {.arg dat_input} and {.arg par_input} are required inputs.",
           missing_set_mappings = "Set mappings passed to {.arg ...} as a pairwise list are required.",
           invalid_internal_mapping = c("The internal mapping selected: {.val {set_map}}, for set {.val {map_name}} does not exist.",
                                        "Available internal mappings for {.val {map_name}} include {.val {available_mappings}}"),
           missing_ele_ext_mapping = "The set mapping for {.val {map_name}} is missing mappings for {.val {missing_ele}}.",
           no_ele_ext_mapping = "No required elements were found for the {.field {data_format}} set {.val {map_name}} indicating an invalid set name.",
           no_internal_mapping = "No internal {.field {data_format}} mappings were found for the set {.val {map_name}}."
          )
    }
  ),

  tar_target(load_wrn,
             {
               list(extra_input = "If {.arg eqm_input} is provided, {.arg dat_input}, {.arg par_input}, and {.arg set_input} arguments are unnecessary.")
             }),

  tar_target(
    cls_err,
    {
      list(invalid_internal = c("The closure file inferred from the provided {.arg tab_file}: {.val {file}} does not exist.",
                                "Currently supported internal {file_type} files are available for: {.val {file_names}}.",
                                "Alternatively, path to a user-provided {file_type} file is supported (e.g., \"/my/{file_type}/path.{ext}\")",
                                "Note that user-provided {file_type} files may need to be modified for compatibility with various {.pkg teems} functions."),
           no_var = "{l_var} variable{?s} from the closure file not present in the Tablo file: {.val {var_discrepancy}}.")
    }
  ),

  tar_target(
    shk_err,
    {
      list(cst_scen_val_file = "The last column in the loaded file {.file {input}} must be a {.field Value} column.",
           cst_scen_val_df = "{.obj_type_friendly {input}} supplied as a shock must have {.field Value} as the last column.",
           scen_year_file = "No {.field Year} column was found in the loaded file {.file {input}}.",
           scen_year_df = "{.obj_type_friendly {input}} supplied as a scenario shock must have {.field Year} as the last column.",
           uni_named_lst = "{.arg ...} must consist of named pairs in the format {.code SETi = set_element}",
           unneeded_dots = "{.arg ...} are only utilized for shock type {.val uniform}.",
           not_a_shk = "The value provided to {.arg shock} is not an object created with {.fun teems::emsshock}.",
           not_a_var = "The variable designated for a shock: {.val {var_name}}  was not found within the model Tablo file.",
           extra_col = "If {.field Year} is provided in lieu of the intertemporal set, the intertemporal set {.field {supplied_int_set}} is not necessary.",
           invalid_set = c("{l_errant_set} set{?s} designated for an element-specific uniform shock: {.field {errant_set}} not applicable to the variable {.val {var_name}}.",
                           "Set designations within {.pkg teems} comprise the variable-specific uppercase set name and lowercase index.",
                           "For {.val {var_name}} these include: {.field {ls_mixed}}.",
                           "In intertemporal models, {.field Year} may be provided in lieu of an intertemporal set."),
           x_full_exo = "The variable {.val {var_name}} was assigned a shock over the entire variable yet is not fully exogenous.",
           uni_invalid_year = "The Year provided for a shock {.val {Year}} is not among years consistent with provided time steps {.field {CYRS}}.",
           uni_invalid_ele = c("The element: {.val {depurred.ele}} is not found within the associated set: {.field {depurred.ele_set}}",
                               "Valid elements with the current mapping include: {.val {recognized_ele}}."),
           x_part_exo = "The following tuples have been allocated a shock but are not exogenous: {.field {errant_tup}}.",
           cust_invalid_year = "{n_errant_year_tuples} tuple{?s} in the provided custom shock file contain invalid {.field Year} specifications: {.field {errant_year_typles}}.",
           cust_invalid_tup = "Some tuples provided to a {.arg custom} shock indicate elements used that do not belong to their respective sets: {.field {errant_tuples}}.",
           cust_endo_tup = "Some tuples designated for a shock do not have exogenous status: {.field {x_exo_parts}}.",
           scen_dynamic = "Shock type {.arg scenario} is only valid for temporally dynamic models.",
           scen_missing_tup = c("{n_missing_tuples} tuple{?s} in the provided scenario shock file were missing: {.field {missing_tuples}}.",
                                "Note that scenario shocks are subject to aggregation and must contain all unaggregated database- and variable-specific elements for associated sets."),
           shk_file_shocks = "No additional shocks are accepted if a shock file is provided."
      )
    }
  ),

  tar_target(
    shk_infm,
    {
      list(uni_named_lst = "Note that set names consist of the concatenation of the set name and variable-specific lowercase index.")
    }
  ),

  tar_target(
    shk_url,
    {
      list(custom = NULL,
           scenario = NULL,
           uniform = NULL)
    }
  ),

  tar_target(
    swap_err,
    {
      list(invalid_swap = "Invalid list object supplied as swap.",
           no_var = "The variable {.val {var_name}} designated for a swap {direction} is not found within the model Tablo file provided.",
           invalid_set = c("The swap-{direction} set {.val {non_exist_set}} is not associated with the variable {.val {var_name}}.",
                           "Note that set designations within {.pkg teems} are comprised of the variable-specific uppercase set name and lowercase index.",
                           "For {.val {var_name}} these include: {.field {ls_mixed}}."))
    }
  ),

  # internal data ====================================================
  tar_target(
    internal_data,
    {
      usethis::use_data(mappings,
        internal_tab,
        #`GTAP-INTv2`,
        param_weights,
        internal_cls,
        set_table,
        set_conversion,
        coeff_conversion,
        gen_info,
        gen_wrn,
        gen_err,
        gen_url,
        data_err,
        load_err,
        shk_err,
        swap_err,
        cls_err,
        shk_infm,
        shk_url,
        overwrite = TRUE,
        internal = TRUE
      )
    }
  )

  # external data ====================================================
  # teems_dat()

  # tab files
  # readLines used instead of readChar for readability (user-facing)
  # tar_target(external_tab,
  #            {
  #              tabs <- lapply(X = tab_files,
  #                             FUN = readLines)
  #
  #              names(x = tabs) <- tab_names
  #              return(tabs)
  #            }),
  #
  # tar_target(external_data,
  #            {
  #              purrr::map2(.x = external_tab,
  #                          .y = names(external_tab),
  #                          .f = function(t, n) {
  #                            file_path <- paste0("./data/", n, ".rda")
  #                            save(t, file = file_path)
  #                          })
  #            })
)