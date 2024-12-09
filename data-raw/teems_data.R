library(targets)

targets::tar_config_set(store = "./data-raw/_targets")

# Set target options:
targets::tar_option_set(
  packages = c("data.table", "usethis", "purrr"),
  format = "qs",
  cue = tar_cue("always")
)



# functions
targets::tar_source("./data-raw/R")

list(
  tar_target(name = db_version,
             command = c("v9", "v10", "v11")),
  # mapping related --------------------------------------------------
  tar_target(name = mapping_files,
             command = {
               list.files(path = file.path("data-raw", "mappings", db_version),
                          full.names = TRUE,
                          recursive = TRUE)
             },
             pattern = map(db_version),
             format = "file"),

  tar_target(name = mappings,
             command = process_mappings(path = mapping_files,
                                        db_version = db_version)),
  # tab related ------------------------------------------------------
  tar_target(name = tab_files,
             command = {
               list.files(path = file.path("data-raw", "tab_files"),
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
                        tablo <- teems:::.parse_tablo(tab)
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
  tar_target(name = v62_weights,
             command = {
               list(ESBD = c("VDPA", "VIPA", "VDGA", "VIGA", "VDFA", "VIFA"),
                    ESBM = c("VIPA", "VIGA", "VIFA"),
                    ESBT = c("VDFM", "VIFM", "VFM", "FTRV", "-FBEP", "-ISEP"),
                    ESBV = "EVFA",
                    INCP = c("VDPA", "VIPA"),
                    SUBP = c("VDPA", "VIPA"))
             }),

  tar_target(name = v7_weights,
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
               list(v62 = v62_weights, v7 = v7_weights)
             }),

  # intertemporal ====================================================
  tar_target(name = v1_int_param,
             command = {
               list(
                 CPHI = list(
                   header = "CPHI",
                   information = "investment adjustment parameter",
                   coeff = "CPHI",
                   v_class = "rate",
                   sets = c("REGr", "ALLTIMEt")
                 ),
                 INID = list(
                   header = "INID",
                   information = "binary switch mechanism for initial capital condition",
                   coeff = "INIDELTA",
                   v_class = "switch",
                   sets = "null_set"
                 ),
                 IRAT = list(
                   header = "IRAT",
                   information = "long-term interest rate",
                   coeff = "LRORG",
                   v_class = "rate",
                   sets = "ALLTIMEt"
                 ),
                 KAPP = list(
                   header = "KAPP",
                   information = "depreciation rate",
                   coeff = "KAPPA",
                   v_class = "rate",
                   sets = c("REGr", "ALLTIMEt")
                 )
               )
             }),

  tar_target(name = v2_int_param,
             command = NULL),

  tar_target(name = int_params,
             command = {
               list(v1 = v1_int_param)
             }),

  # closures ---------------------------------------------------------
  tar_target(name = closure_files,
             command = {
               list.files(path = "./data-raw/closures", full.names = TRUE)
             },
             format = "file"),

  tar_target(name = closures,
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
             command = "./data-raw/mappings/v9/REG/big3.csv",
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
             command = "./data-raw/mappings/v9/TRAD_COMM/macro_sector.csv",
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
             command = "./data-raw/mappings/v9/ENDW_COMM/labor_agg.csv",
             format = "file"),

  tar_target(name = endw_labagg,
             command = read.csv(endw_labagg_file)),

  # teems_shock()
  tar_target(name = afeall_v62,
             command = {
               afeall <- expand.grid(ENDW_COMMi = unique(endw_labagg[["mapping"]]),
                                     PROD_COMMj = c(unique(sector_macro[["mapping"]]), "zcgds"),
                                     REGr = unique(reg_big3[["mapping"]]))

               afeall[["Value"]] <- runif(n = nrow(x = afeall), max = 2)
               return(afeall)
             }),

  tar_target(name = custom_shk,
             command = {
               fwrite(afeall_v62, "./inst/extdata/custom_shk.csv")
             },
             format = "file"),

  tar_target(name = targeted_shk,
             command = {
               afeall <- afeall_v62[afeall_v62$REGr == "usa",]
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
  tar_target(name = GTAPv62_cls_file,
             command = "./data-raw/closures/GTAPv62.cls",
             format = "file"),

  tar_target(name = GTAPv62_cls,
             command = {
               readLines(GTAPv62_cls_file)
             }),

  tar_target(name = user_cls,
             command = {
               user_cls <- sub(pattern = "incomeslack",
                               replacement = "y",
                               x = GTAPv62_cls)
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

  # internal data ====================================================
  tar_target(name = internal_data,
             command = {
               usethis::use_data(mappings,
                                 internal_tab,
                                 var_extracts,
                                 param_weights,
                                 int_params,
                                 closures,
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
