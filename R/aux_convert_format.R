#' @importFrom purrr pluck
#' @importFrom data.table CJ copy rbindlist setnames merge.data.table
#' 
#' @keywords internal
#' @noRd
.convert_format <- function(data,
                            data_format) {

  data_type <- attr(x = ls_array, "data_type")
  
  if (identical(x = data_format, y = "v6.2")) {
    if (identical(x = data_type, y = "set")) {

      data <- lapply(X = data,
                     FUN = function(header) {
                       .convert_id(header = header,
                                   table = set_conversion,
                                   origin = data_format,
                                   colname = TRUE)
                     })

      ACTS <- purrr::pluck(.x = data, "H2")
      ACTS[["header_name"]] <- "ACTS"
      colnames(ACTS[["dt"]]) <- "ACTS"
      ACTS <- list(ACTS = ACTS)

      data <- c(data, ACTS)
    } else if (identical(x = data_type, y = "par")) {
      # don't see any drawback to using the regions from here rather than bringing forth the prior set object
      REGr <- purrr::pluck(.x = data, "RFLX", "dt", "REGr")
      data <- lapply(X = data,
                     FUN = function(header) {
                       dt <- header[["dt"]]
                       nme <- header[["header_name"]]
                       v7.0_colnames <- unlist(x = subset(x = param_conversion,
                                                          subset = {is.element(el = v6.2header,
                                                                               set = nme)},
                                                          select = v7.0set))
                       if (is.element(el = nme, set = c("ESBD", "ESBM", "ETRE", "ESBT", "ESBV"))) {
                         set_col <- setdiff(x = colnames(x = dt), y = "Value")
                         new_dt <- dt[, .(REGr, Value), by = set_col]
                         if (is.element(el = nme, set = c("ESBT", "ESBV"))) {
                           new_dt <- new_dt[ACTSa != "zcgds",]
                         }
                       }

                       if (exists(x = "new_dt")) {
                       header[["dt"]] <- new_dt
                       }

                       return(header)
                     })

      # add missing v7 parameters
      REG <- REGr
      ACTS <- unique(purrr::pluck(.x = data, "ESBV", "dt", "ACTSa"))
      COMM <- ACTS
      MARG <- c("atp", "otp", "wtp")

      missing_v7.0 <- unlist(x = subset(x = param_conversion,
                             subset = {is.na(x = v6.2header)},
                             select = v7.0header))

      # should pass in via fun rather than parent.frame
      missing_v7.0 <- with(data = param_conversion,
           expr = {
             lapply(X = missing_v7.0,
                    FUN = function(nme) {
                      r_idx <- grep(pattern = nme, x = v7.0header)
                      sets <- v7.0set[r_idx][[1]]
                      #descr <- v7.0description[r_idx][[1]]
                      set_ele <- mget(x = sets, parent.frame(n = 5))
                      lower_idx <- tolower(x = substr(x = names(x = set_ele), start = 1, stop = 1))
                      names(x = set_ele) <- paste0(names(x = set_ele), lower_idx)

                      if (is.element(el = nme, set = c("ESBG", "ESBS"))) {
                        value <- 1
                      } else if (identical(x = nme, y = "ETRQ")) {
                        value <- -5
                      } else if (is.element(el = nme, set = c("ESBC", "ESBQ"))) {
                        value <- 0
                      }

                      dt <- do.call(what = data.table::CJ,
                                    args = c(set_ele, Value = value))

                      list(header_name = nme,
                           #information = descr,
                           dt = dt)
                    })
           })

      names(x = missing_v7.0) <- sapply(X = missing_v7.0,
                                        FUN = function(x){x[["header_name"]]})
      data <- c(data, missing_v7.0)
    } else if (identical(x = data_type, y = "dat")) {
      # TVOM has MAKB and MAKS (net OSEP)
      # missing headers (MAKS, MAKB, VDIB, VDIP, VMIP, VMIB, CSEP)
      missing_v7.0 <- unlist(x = subset(x = base_conversion,
                                        subset = {is.na(x = v6.2header)},
                                        select = v7.0header))

      missing_v7.0 <- with(data = base_conversion,
                           expr = {
                             lapply(X = missing_v7.0,
                                    FUN = function(nme) {
                                      if (identical(x = nme, y = "VDIB")) {
                                         dt <- data.table::copy(x = purrr::pluck(.x = data, "VDFM", "dt"))
                                         dt <- dt[ACTSa == "zcgds", !("ACTSa"), with = FALSE]
                                      } else if (identical(x = nme, y = "VDIP")) {
                                        dt <- data.table::copy(x = purrr::pluck(.x = data, "VDFA", "dt"))
                                        dt <- dt[ACTSa == "zcgds", !("ACTSa"), with = FALSE]
                                      } else if (identical(x = nme, y = "VMIB")) {
                                        dt <- data.table::copy(x = purrr::pluck(.x = data, "VIFM", "dt"))
                                        dt <- dt[ACTSa == "zcgds", !("ACTSa"), with = FALSE]
                                      } else if (identical(x = nme, y = "VMIP")) {
                                        dt <- data.table::copy(x = purrr::pluck(.x = data, "VIFA", "dt"))
                                        dt <- dt[ACTSa == "zcgds", !("ACTSa"), with = FALSE]
                                      } else if (identical(x = nme, y = "CSEP")) {
                                        dt <- data.table::copy(x = purrr::pluck(.x = data, "ISEP", "dt"))
                                        dt <- dt[ACTSa != "zcgds"]
                                        dt[, Value := Value * -1]
                                      } else if (is.element(el = nme, set = c("MAKB", "MAKS"))) {
                                        dt <- data.table::copy(x = purrr::pluck(.x = data, "TVOM", "dt"))
                                        dt[, ACTSa := COMMc]
                                        sectors <- unique(x = dt[["COMMc"]])
                                        null_dt <- data.table::CJ(COMMc = sectors,
                                                                  ACTSa = sectors,
                                                                  REGr = unique(x = dt[["REGr"]]),
                                                                  Value = 0)
                                        null_dt <- null_dt[COMMc != ACTSa,]
                                        dt <- data.table::rbindlist(l = list(null_dt, dt), use.names = TRUE)
                                        if (identical(x = nme, y = "MAKS")) {
                                          OSEP <- data.table::copy(x = purrr::pluck(.x = data, "OSEP", "dt"))
                                          OSEP[, ACTSa := COMMc]
                                          data.table::setnames(x = OSEP, old = "Value", new = "OSEP")
                                          dt <- data.table::merge.data.table(x = dt,
                                                                             y = OSEP,
                                                                             by = c("COMMc", "ACTSa", "REGr"),
                                                                             all.x = TRUE)
                                          dt[, Value := Value + OSEP]
                                          dt[is.na(x = Value), Value := 0]
                                          dt[, OSEP := NULL]
                                        }
                                      }

                                      list(header_name = nme,
                                           dt = dt)
                                    })})
      data <- lapply(
        X = data,
        FUN = function(header) {
          dt <- header[["dt"]]
          nme <- header[["header_name"]]

          if (is.element(el = nme, set = c("EVFA",
                                           "VFM",
                                           "FBEP",
                                           "FTRV",
                                           "FBEP",
                                           "VDFA",
                                           "VDFM",
                                           "VIFA",
                                           "VIFM"))) {
            new_dt <- dt[ACTSa != "zcgds"]
          } else if (identical(x = nme, y = "EVOA")) {
            # use EVOA with shared out VFM PROD_COMM to recreate EVOS
            VFM <- data.table::copy(purrr::pluck(.x = data, "VFM", "dt"))
            col_names <- colnames(x = VFM)
            VFM <- VFM[ACTSa != "zcgds",]
            VFM_totals <- VFM[, .(total = sum(Value)), by = c("ENDWe", "REGr")]
            VFM <- data.table::merge.data.table(x = VFM,
                                                y = VFM_totals,
                                                by = c("ENDWe", "REGr"))
            VFM[, phi := Value / total]
            dt <- data.table::merge.data.table(x = dt,
                                               y = VFM[, c("ENDWe", "REGr", "ACTSa", "phi")])
            dt[, Value := Value * phi]
            new_dt <- dt[, phi := NULL]
            data.table::setcolorder(x = new_dt, neworder = col_names)
          } else if (identical(x = nme, y = "ISEP")) {
            new_dt <- dt[ACTSa == "zcgds"]
            new_dt <- new_dt[, let(ACTSa = NULL, Value = Value * -1)]
          }

          if (exists(x = "new_dt")) {
            header[["dt"]] <- new_dt
          }

          return(header)
        }
      )

      data <- lapply(X = data,
                     FUN = function(header) {
                       .convert_id(header = header,
                                   table = base_conversion,
                                   origin = data_format)
                     })
      data <- c(data, missing_v7.0)
    }
  } else if (identical(x = data_format, y = "v7.0")) {
    if (identical(x = data_type, y = "set")) {
      # v7.0 to v6.2 on sets involves changing set names
      data <- lapply(X = data,
                     FUN = function(header) {
                       .convert_id(header = header,
                                   table = set_conversion,
                                   origin = data_format,
                                   colname = TRUE)
                     })

      # add missing v6.2 set
      missing_v6.2 <- list(H9 = list(header_name = "H9",
                                     #information = "Set CGDS_COMM  capital goods commodities",
                                     dt = data.table::data.table(H9 = "zcgds")))

      data <- c(data, missing_v6.2)
    } else if (identical(x = data_type, y = "par")) {
      # v7.0 to v6.2 on parameters involves summing over the additional (uniform) sets and adding zcgds
      data <- lapply(X = data,
        FUN = function(header) {
          dt <- header[["dt"]]
          nme <- header[["header_name"]]
          v6.2_colnames <- unlist(x = subset(x = param_conversion,
                                 subset = {is.element(el = v7.0header,
                                                      set = nme)},
                                 select = v6.2set))

          drop_val <- colnames(x = dt)[!is.element(el = colnames(x = dt), set = "Value")]
          drop_col <- drop_val[!is.element(el = substring(
            text = drop_val,
            first = 1,
            last = nchar(drop_val) - 1
          ),
          set = v6.2_colnames)]

          dt <- dt[, !(drop_col), with = FALSE]
          dt <- unique(x = dt)

          # CGDS additions
          if (identical(x = nme, y = "ESBT")) {
            cgds_row <- list(PROD_COMMj = "zcgds", Value = 0)
            dt <- rbind(dt, cgds_row)
          }

          if (identical(x = nme, y = "ESBV")) {
            cgds_row <- list(PROD_COMMj = "zcgds", Value = 1)
            dt <- rbind(dt, cgds_row)
          }

          header[["dt"]] <- dt
          return(header)
        }
      )
    } else if (identical(x = data_type, y = "dat")) {
      # v7.0 to v6.2 on base involves changing names, adding zcgds, and other op
      data <- lapply(
        X = data,
        FUN = function(header) {
          dt <- header[["dt"]]
          nme <- header[["header_name"]]

          if (identical(x = nme, y = "VDFB")) {
            cgds <- data.table::copy(purrr::pluck(data, "VDIB", "dt"))
            cgds[, let(PROD_COMMj = "zcgds")]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VDFP")) {
            cgds <- data.table::copy(purrr::pluck(data, "VDIP", "dt"))
            cgds[, let(PROD_COMMj = "zcgds")]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VMFB")) {
            cgds <- data.table::copy(purrr::pluck(data, "VMIB", "dt"))
            cgds[, let(PROD_COMMj = "zcgds")]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VMFP")) {
            cgds <- data.table::copy(x = purrr::pluck(data, "VMIP", "dt"))
            cgds[, let(PROD_COMMj = "zcgds")]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (is.element(el = nme,
                                set = c("EVFB", "EVFP", "FBEP", "FTRV"))) {
            cgds <- data.table::CJ(
              ENDW_COMMi = unique(dt[["ENDW_COMMi"]]),
              PROD_COMMj = "zcgds",
              REGr = unique(dt[["REGr"]]),
              Value = 0
            )
            new_dt <- data.table::rbindlist(l = list(dt, cgds))
          } else if (identical(x = nme, y = "EVOS")) {
            new_dt <- dt[, .(Value = sum(Value)), by = c("ENDW_COMMi", "REGr")]
          } else if (identical(x = nme, y = "ISEP")) {
            cgds <- data.table::copy(x = dt)
            cgds[, let(PROD_COMMj = "zcgds", Value = Value * -1)]
            dt <- data.table::copy(purrr::pluck(data, "CSEP", "dt"))
            dt[, let(Value = Value * -1)]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          }

          if (exists(x = "new_dt")) {
          header[["dt"]] <- new_dt
          }

          return(header)
        }
      )
      data <- lapply(X = data,
                     FUN = function(header) {
                       .convert_id(header = header,
                                   table = base_conversion,
                                   origin = data_format)
                     })
    }


  }
  names(x = data) <- sapply(X = data,
                           FUN = function(x){x[["header_name"]]})
  
  attr(x = data, which = "data_type") <- data_type
  return(data)
}
