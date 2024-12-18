.convert_data_format <- function(data,
                                 data_format,
                                 data_type) {

  if (identical(x = data_format, y = "v6.2")) {
    if (identical(x = data_type, y = "set")) {
browser()
      data <- lapply(
        X = data,
        FUN = function(header) {
          browser()
          new_name <- set_conversion[grep(pattern = header[["header_name"]],
                                          x = set_conversion[["v6.2header"]]), "v7.0header"]
          if (!identical(x = new_name, y = character(0)) && !is.na(x = new_name)) {
            header[["header_name"]] <- new_name
            colnames(header[["dt"]]) <- new_name
          }
          return(header)
        }
      )

      ACTS <- purrr::pluck(.x = data, "H2")
      ACTS[["header_name"]] <- "ACTS"
      colnames(ACTS[["dt"]]) <- "ACTS"

      # index here? information?
      names(x = data) <- sapply(X = data,
                                FUN = function(x){x[["header_name"]]})
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
                           new_dt <- new_dt[ACTSa != "CGDS",]
                         }
                       }

                       if (exists(x = "new_dt")) {
                       header[["dt"]] <- new_dt
                       }

                       return(header)
                     }
      )
      # add missing v7 parameters
      REG <- REGr
      ACTS <- unique(purrr::pluck(.x = data, "ESBV", "dt", "ACTSa"))
      MARG <- c("atp", "otp", "wtp")

      missing_v7.0 <- unlist(x = subset(x = param_conversion,
                             subset = {is.na(v6.2header)},
                             select = v7.0header))

      missing_v7.0 <- with(data = param_conversion,
           expr = {
             lapply(X = missing_v7.0,
                    FUN = function(nme) {
                      r_idx <- grep(pattern = nme, x = v7.0header)
                      sets <- v7.0set[r_idx][[1]]
                      descr <- v7.0description[r_idx][[1]]
                      set_ele <- mget(x = sets, parent.frame(n = 5))
                      lower_idx <- tolower(x = substr(x = names(x = set_ele), start = 1, stop = 1))
                      names(x = set_ele) <- paste0(names(x = set_ele), lower_idx)

                      if (is.element(el = nme, set = c("ESBG", "ESBS"))) {
                        value <- 1
                      } else if (identical(x = nme, y = "ETRQ")) {
                        value <- -5
                      } else if (identical(x = nme, y = "ESBC")) {
                        value <- 0
                      }

                      dt <- do.call(what = data.table::CJ,
                                    args = c(set_ele, Value = value))

                      list(header_name = nme,
                           information = descr,
                           dt = dt)
                    })
           })

      names(x = missing_v7.0) <- sapply(X = missing_v7.0,
                                        FUN = function(x){x[["header_name"]]})
      data <- c(data, missing_v7.0)
    } else if (identical(x = data_type, y = "dat")) {
      browser()
    }
  } else if (identical(x = data_format, y = "v7.0")) {
    if (identical(x = data_type, y = "set")) {
      # v7.0 to v6.2 on sets involves changing set names
      data <- lapply(
        X = data,
        FUN = function(header) {
          new_name <- set_conversion[grep(pattern = header[["header_name"]],
                                          x = set_conversion[["v7.0header"]]), "v6.2header"]
          if (!identical(x = new_name, y = character(0)) && !is.na(x = new_name)) {
            header[["header_name"]] <- new_name
            colnames(header[["dt"]]) <- new_name
          }
          return(header)
        }
      )

      # add missing v6.2 set
      missing_v6.2 <- list(H9 = list(header_name = "H9",
                                     information = "Set CGDS_COMM  capital goods commodities",
                                     dt = data.table::data.table(H9 = "cgds")))

      data <- c(data, missing_v6.2)
      names(x = data) <- sapply(X = data,
                                FUN = function(x){x[["header_name"]]})
    } else if (identical(x = data_type, y = "par")) {
      # v7.0 to v6.2 on parameters involves summing over the additional (uniform) sets and adding cgds
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

          dt[, (drop_col) := NULL]
          dt <- unique(x = dt)

          # CGDS additions
          if (identical(x = nme, y = "ESBT")) {
            cgds_row <- list(PROD_COMMj = "cgds", Value = 0)
            dt <- rbind(dt, cgds_row)
          }

          if (identical(x = nme, y = "ESBV")) {
            cgds_row <- list(PROD_COMMj = "cgds", Value = 1)
            dt <- rbind(dt, cgds_row)
          }

          header[["dt"]] <- dt
          return(header)
        }
      )
    } else if (identical(x = data_type, y = "dat")) {
      # v7.0 to v6.2 on base involves changing names, adding cgds, and other op
      data <- lapply(
        X = data,
        FUN = function(header) {
          dt <- header[["dt"]]
          nme <- header[["header_name"]]

          # see if headers differ
          v6.2_name <- unlist(x = subset(
            x = base_conversion,
            subset = {
              is.element(el = v7.0header, set = nme)
            },
            select = v6.2header
          ))

          if (identical(x = nme, y = "VDFB")) {
            cgds <- data.table::copy(purrr::pluck(data, "VDIB", "dt"))
            cgds[, let(PROD_COMMj = "cgds")]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VDFP")) {
            cgds <- data.table::copy(purrr::pluck(data, "VDIP", "dt"))
            cgds[, let(PROD_COMMj = "cgds")]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VMFB")) {
            cgds <- data.table::copy(purrr::pluck(data, "VMIB", "dt"))
            cgds[, let(PROD_COMMj = "cgds")]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VMFP")) {
            cgds <- data.table::copy(x = purrr::pluck(data, "VMIP", "dt"))
            cgds[, let(PROD_COMMj = "cgds")]
            new_dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (is.element(el = nme,
                                set = c("EVFB", "EVFP", "FBEP", "FTRV"))) {
            cgds <- data.table::CJ(
              ENDW_COMMi = unique(dt[["ENDW_COMMi"]]),
              PROD_COMMj = "cgds",
              REGr = unique(dt[["REGr"]]),
              Value = 0
            )
            new_dt <- data.table::rbindlist(l = list(dt, cgds))
          } else if (identical(x = nme, y = "EVOS")) {
            new_dt <- dt[, .(Value = sum(Value)), by = c("ENDW_COMMi", "REGr")]
          } else if (identical(x = nme, y = "ISEP")) {
            cgds <- data.table::copy(x = dt)
            cgds[, let(PROD_COMMj = "cgds", Value = Value * -1)]
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

      data <- lapply(
        X = data,
        FUN = function(header) {
          new_name <- base_conversion[grep(pattern = header[["header_name"]],
                                            x = base_conversion[["v7.0header"]]), "v6.2header"]
          if (!identical(x = new_name, y = character(0)) && !is.na(x = new_name)) {
            header[["header_name"]] <- new_name
          }
          return(header)
        }
      )

      names(x = data) <- sapply(X = data,
                                FUN = function(x){x[["header_name"]]})
    }


  }
  return(data)
}
