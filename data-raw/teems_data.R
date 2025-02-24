library(targets)

targets::tar_config_set(store = "./data-raw/_targets")

# Set target options:
targets::tar_option_set(
  packages = c("data.table", "usethis", "purrr", "tabulapdf", "dplyr"),
  format = "qs",
  cue = tar_cue("always")
)



# functions
targets::tar_source("./data-raw/R")

list(
  tar_target(name = db_version,
             command = c("v9", "v10", "v11")),
  tar_target(name = data_format,
             command = c("v6.2", "v7.0")),
  # mapping related --------------------------------------------------
  tar_target(name = mapping_repo,
             command = {
               list.files(path = file.path("../teems-mappings/", db_version),
                          full.names = TRUE,
                          recursive = TRUE)
             },
             pattern = map(db_version),
             format = "file"),
  
  tar_target(name = mapping_files,
             command = {
               package_mappings <- file.path("data-raw", "mappings")
               file.copy(from = mapping_repo,
                         to = package_mappings,
                         recursive = T)
               list.files(path = package_mappings,
                          recursive = TRUE,
                          full.names = TRUE)
             },
             format = "file"),

  tar_target(name = mappings,
             command = process_mappings(path = mapping_files,
                                        db_version = db_version,
                                        data_format = data_format)),
  # tab related ------------------------------------------------------
  tar_target(name = tab_repo,
             command =  list.files(path = file.path("../teems-tabs/"),
                                   full.names = TRUE),
             format = "file"),
  
  tar_target(name = tab_files,
             command = {
               package_tabs <- file.path("data-raw", "tab_files")
               file.copy(from = tab_repo,
                         to = package_tabs,
                         recursive = TRUE)
               list.files(path = package_tabs,
                          full.names = TRUE)
             },
             format = "file"),

  tar_target(name = tab_names,
             command = {
               lapply(tab_files, function(tab) {
                 tools::file_path_sans_ext(x = basename(path = tab))
               })
             }),

  tar_target(name = internal_tab,
             command = {
               tabs <- lapply(X = tab_files,
                      FUN = function(tab) {
                        readChar(
                          con = tab,
                          nchars = file.info(tab)[["size"]]
                        )
                      })

               names(x = tabs) <- tab_names
               return(tabs)
             }),


  tar_target(name = parsed_tabs,
             command = {
               lapply(X = tab_files,
                      FUN = function(tab) {
                        tablo <- teems:::.parse_tablo(tab,
                                                      model_version = NULL)
                        tablo[["extract"]]
                      })
             }),

  tar_target(name = var_extracts,
             command = {
               extract <- lapply(X = parsed_tabs,
                                 FUN = teems:::.tablo_variables)
               names(x = extract) <- tab_names
               return(extract)
             }),

  # parameter related ------------------------------------------------
  # static ===========================================================
  tar_target(name = v6.2_weights,
             command = {
               list(ESBD = c("VDPA", "VIPA", "VDGA", "VIGA", "VDFA", "VIFA"),
                    ESBM = c("VIPA", "VIGA", "VIFA"),
                    ESBT = c("VDFM", "VIFM", "VFM", "FTRV", "-FBEP", "-ISEP"),
                    ESBV = "EVFA",
                    INCP = c("VDPA", "VIPA"),
                    SUBP = c("VDPA", "VIPA"))
             }),

  tar_target(name = v7.0_weights,
             command = {
               list(ESBD = c("VDPP", "VMPP", "VMGP", "VDGP", "VDFP", "VMFP"),
                    ESBM = c("VMPP", "VMGP", "VMFP"),
                    ESBT = c("VDFB", "VMFB", "EVFB", "FTRV", "-FBEP", "-ISEP"),
                    ESBV = "EVFP",
                    INCP = c("VDPP", "VMPP"),
                    SUBP = c("VDPP", "VMPP"))
             }),

  tar_target(name = param_weights,
             command = {
               list(v6.2 = v6.2_weights, v7.0 = v7.0_weights)
             }),

  # closures ---------------------------------------------------------
  tar_target(name = closure_repo,
             command = {
               list.files(path = file.path("../teems-closures/"),
                          full.names = TRUE)
               
             }),
  
  tar_target(name = closure_files,
             command = {
               package_closures <- file.path("data-raw", "closures")
               file.copy(from = closure_repo,
                         to = package_closures,
                         recursive = T)
               list.files(path = package_closures,
                          recursive = TRUE,
                          full.names = TRUE)
             },
             format = "file"),

  tar_target(name = internal_cls,
             command = {
               cls <- lapply(X = closure_files,
                             FUN = function(closure) {
                               readLines(closure)
                             })

               cls_names <- lapply(closure_files, function(cls) {
                 tools::file_path_sans_ext(x = basename(path = cls))
               })

               names(x = cls) <- cls_names
               return(cls)
             }),

  # inst/ext data -------------------------------------------------------------
  # teems_dat()
  tar_target(name = SAVE_file,
             command = "./data-raw/examples/SAVE.csv",
             format = "file"),

  tar_target(name = SAVE_data,
             command = {
               SAVE <- read.csv(file = SAVE_file)
               fwrite(SAVE, file = "./inst/extdata/SAVE.csv")
             },
             format = "file"),

  tar_target(name = SAVE_preagg,
             command = {
               SAVE <- read.csv(file = SAVE_file)
               set.seed(seed = 1789)
               SAVE[["Value"]] <- runif(n = nrow(SAVE),
                                        min = min(SAVE[["Value"]]),
                                        max = max(SAVE[["Value"]]))

               fwrite(SAVE, file = "./inst/extdata/SAVE_preagg.csv")
             },
             format = "file"),

  tar_target(name = reg_big3_file,
             command = "./data-raw/mappings/v9/v6.2/REG/big3.csv",
             format = "file"),

  tar_target(name = reg_big3,
             command = {
               read.csv(file = reg_big3_file)
             }),

  tar_target(name = SAVE_postagg,
             command = {
               SAVE <- read.csv(file = SAVE_file)
               r_idx <- match(x = SAVE[["REGr"]], table = reg_big3[["H1"]])
               SAVE[["REGr"]] <- reg_big3[["mapping"]][r_idx]
               SAVE <- aggregate(Value ~ REGr, data = SAVE, FUN = sum)

               fwrite(SAVE, file = "./inst/extdata/SAVE_postagg.csv")
             },
             format = "file"),

  # teems_param()
  tar_target(name = SUBP_file,
             command = "./data-raw/examples/SUBP.csv",
             format = "file"),

  tar_target(name = SUBP_data,
             command = {
               SUBP <- read.csv(file = SUBP_file)
               fwrite(SUBP, file = "./inst/extdata/SUBP.csv")
             },
             format = "file"),

  tar_target(name = SUBP_preagg,
             command = {
               SUBP <- read.csv(file = SUBP_file)
               set.seed(seed = 1859)
               SUBP[["Value"]] <- runif(n = nrow(SUBP),
                                        min = min(SUBP[["Value"]]),
                                        max = max(SUBP[["Value"]]))

               fwrite(SUBP, file = "./inst/extdata/SUBP_preagg.csv")
             },
             format = "file"),

  tar_target(name = sector_macro_file,
             command = "./data-raw/mappings/v9/v6.2/TRAD_COMM/macro_sector.csv",
             format = "file"),

  tar_target(name = sector_macro,
             command = read.csv(sector_macro_file)),

  tar_target(name = SUBP_postagg,
             command = {
               SUBP <- read.csv(file = SUBP_file)
               r_idx <- match(x = SUBP[["REGr"]], table = reg_big3[["H1"]])
               SUBP[["REGr"]] <- reg_big3[["mapping"]][r_idx]

               i_idx <- match(x = SUBP[["TRAD_COMMi"]], table = sector_macro[["H2"]])
               SUBP[["TRAD_COMMi"]] <- sector_macro[["mapping"]][i_idx]
               SUBP <- aggregate(Value ~ TRAD_COMMi + REGr, data = SUBP, FUN = sum)

               fwrite(SUBP, file = "./inst/extdata/SUBP_postagg.csv")
             },
             format = "file"),

  tar_target(name = usr_macro,
             command = {
               usr_mapping <- read.csv(file = sector_macro_file)
               colnames(usr_mapping) <- c("original_elements", "new_mapping")
               fwrite(usr_mapping, file = "./inst/extdata/user_mapping.csv")
             },
             format = "file"),

  tar_target(name = endw_labagg_file,
             command = "./data-raw/mappings/v9/v6.2/ENDW_COMM/labor_agg.csv",
             format = "file"),

  tar_target(name = endw_labagg,
             command = read.csv(endw_labagg_file)),

  # teems_shock()
  tar_target(name = afeall_v6.2,
             command = {
               afeall <- expand.grid(ENDW_COMMi = unique(endw_labagg[["mapping"]]),
                                     PROD_COMMj = c(unique(sector_macro[["mapping"]]), "zcgds"),
                                     REGr = unique(reg_big3[["mapping"]]))

               afeall[["Value"]] <- runif(n = nrow(x = afeall), max = 2)
               return(afeall)
             }),

  tar_target(name = custom_shk,
             command = {
               fwrite(afeall_v6.2, "./inst/extdata/custom_shk.csv")
             },
             format = "file"),

  tar_target(name = targeted_shk,
             command = {
               afeall <- afeall_v6.2[afeall_v6.2$REGr == "usa",]
               afeall <- afeall[, !(names(afeall) %in% "REGr")]
               fwrite(afeall, "./inst/extdata/targeted_shk.csv")
             },
             format = "file"),

  tar_target(name = scenario_shk_file,
             command = "./data-raw/examples/pop_trajectory.csv",
             format = "file"),

  tar_target(name = scenario_shk,
             command = {
               ssp2 <- read.csv(scenario_shk_file)
               fwrite(ssp2, "./inst/extdata/pop_SSP2.csv")
             },
             format = "file"),

  # teems_closure()
  tar_target(name = GTAPv6.2_cls_file,
             command = {
              closure_files[grepl(pattern = "v6.2", x = closure_files)]
             },
             format = "file"),

  tar_target(name = GTAPv6.2_cls,
             command = {
               readLines(GTAPv6.2_cls_file)
             }),

  tar_target(name = user_cls,
             command = {
               user_cls <- sub(pattern = "incomeslack",
                               replacement = "y",
                               x = GTAPv6.2_cls)
               writeLines(user_cls, "./inst/extdata/user_closure.cls")
             },
             format = "file"),

  # teems_time()
  tar_target(name = user_CPHI,
             command = {
               set.seed(seed = 2025)
               user_CPHI <- expand.grid(REGr = unique(reg_big3[["mapping"]]),
                                        ALLTIMEt = 0:3)
               user_CPHI[["Value"]] <- runif(n = nrow(user_CPHI),
                                             min = 0.05,
                                             max = 0.15)
               fwrite(user_CPHI, "./inst/extdata/user_CPHI.csv")
             },
             format = "file"),

  tar_target(name = user_LRORG,
             command = {
               set.seed(seed = 2100)
               user_LRORG <- data.frame(ALLTIMEt = 0:3,
                                        Value = runif(n = 4, min = 0.01, max = 0.1))
               fwrite(user_LRORG, "./inst/extdata/user_LRORG.csv")
             },
             format = "file"),

  # v6.2 <> v7 conversion
  tar_target(name = GTAPv7_manual,
             command = "./data-raw/aux/Corong and Tsigas - 2017 - The Standard GTAP Model, Version 7.pdf",
             format = "file"),

  tar_target(name = set_table,
             command = {
               tibble::tibble(v6.2_upper = c("TRAD_COMM", "PROD_COMM", "ENDW_COMM", "MARG_COMM", "ALLTIME"),
                              v6.2_mixed = c("TRAD_COMMi", "PROD_COMMj", "ENDW_COMMi", "MARG_COMMm", "ALLTIMEt"),
                              v7.0_upper = c("COMM", "ACTS", "ENDW", "MARG", "ALLTIME"),
                              v7.0_mixed = c("COMMc", "ACTSs", "ENDWe", "MARGm", "ALLTIMEt"))
             }),
  
  # tables here are not even concordance, just semi related lists
  tar_target(name = set_conversion,
             command = {
               set_table <- tabulapdf::extract_tables(file = GTAPv7_manual,
                                                      pages = 83)
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
                             all = TRUE)
             }),

  tar_target(name = param_conversion,
             command = {
               param_table <- tabulapdf::extract_tables(file = GTAPv7_manual,
                                                        pages = 84)

               v7.0_param <- param_table[[1]][, c(5:8)]

               # missing ESBQ
               ESBQ <- tibble::tibble(14, "ESBQ", "COMM*REG", "1/CES elasticity for commodity sourcing")
               colnames(x = ESBQ) <- colnames(x = v7.0_param)

               # missing ESBI
               # ESBI <- tibble::tibble(15, "ESBI", "REG", "Investment expenditure CES elasticity")
               # colnames(x = ESBI) <- colnames(x = v7.0_param)

               v7.0_param <- rbind(v7.0_param, ESBQ)

               v7.0_param <- .table_fix(single = c(11, 12, 27),
                                        double = c(1, 3, 5, 7, 9, 13, 15, 17, 19, 25),
                                        trebble = c(19, 22),
                                        table = v7.0_param,
                                        prefix = "v7.0",
                                        data_type = "par")

               v7.0_param[10:14, "idx"] <- 11:15

               v6.2_param <- param_table[[1]][, c(1:4)]

               v6.2_param <- .table_fix(single = c(11, 12),
                                       double = c(1, 3, 5, 7, 9, 13, 15, 17),
                                       table = v6.2_param,
                                       prefix = "v6.2",
                                       data_type = "par")

               param <- merge(v6.2_param,
                              v7.0_param,
                              by = "idx",
                              all = TRUE)
             }),

  tar_target(name = base_conversion,
             command = {
               coeff_table <- tabulapdf::extract_tables(file = GTAPv7_manual,
                                                        pages = 85:86)

               coeff_table <- data.table::rbindlist(l = coeff_table)

               v7.0_coeff <- coeff_table[, c(4:6)]
               double <- c(3, 18, 20, 22, 24, 26, 30, 32, 35, 37, 41, 49, 51, 54)
               single <- setdiff(seq(1, nrow(v7.0_coeff)), double)
               NAs <- which(is.na(v7.0_coeff[,1]))
               single <- setdiff(single, NAs)
               v7.0_coeff <- .table_fix(single = single,
                                      double = double,
                                      table = v7.0_coeff,
                                      prefix = "v7.0",
                                      data_type = "dat")

               v6.2_coeff <- coeff_table[, c(1:3)]
               double <- c(18, 20, 22, 24, 26, 30, 32, 35, 37, 41)
               single <- setdiff(seq(1, nrow(v6.2_coeff)), double)

               NAs <- which(is.na(v6.2_coeff[,1]))
               single <- setdiff(single, NAs)
               v6.2_coeff <- .table_fix(single = single,
                                       double = double,
                                       table = v6.2_coeff,
                                       prefix = "v6.2",
                                       data_type = "dat")

               coeff <- merge(v6.2_coeff,
                              v7.0_coeff,
                              by = "idx",
                              all = TRUE)

             }),
  # internal data ====================================================
  tar_target(name = internal_data,
             command = {
               usethis::use_data(mappings,
                                 internal_tab,
                                 var_extracts,
                                 param_weights,
                                 internal_cls,
                                 set_table,
                                 set_conversion,
                                 param_conversion,
                                 base_conversion,
                                 overwrite = TRUE,
                                 internal = TRUE)
             })

  # external data ====================================================
  # teems_dat()

  # tab files
  # readLines used instead of readChar for readability (user-facing)
  # tar_target(name = external_tab,
  #            command = {
  #              tabs <- lapply(X = tab_files,
  #                             FUN = readLines)
  #
  #              names(x = tabs) <- tab_names
  #              return(tabs)
  #            }),
  #
  # tar_target(name = external_data,
  #            command = {
  #              purrr::map2(.x = external_tab,
  #                          .y = names(external_tab),
  #                          .f = function(t, n) {
  #                            file_path <- paste0("./data/", n, ".rda")
  #                            save(t, file = file_path)
  #                          })
  #            })

)