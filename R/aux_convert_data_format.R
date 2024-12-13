.convert_data_format <- function(data,
                                 data_format,
                                 data_type) {

  if (identical(x = data_format, y = "v6.2")) {
    browser()
  } else if (identical(x = data_format, y = "v7")) {
    if (identical(x = data_type, y = "set")) {
      # v7 to v62 on sets involves changing set names
      data <- lapply(
        X = data,
        FUN = function(header) {
          new_name <- set_conversion[grep(pattern = header[["header_name"]],
                                          x = set_conversion[["v7header"]]), "v62header"]
          if (!identical(x = new_name, y = character(0)) && !is.na(x = new_name)) {
            header[["header_name"]] <- new_name
          }
          return(header)
        }
      )
      names(x = data) <- sapply(X = data,
                                FUN = function(x){x[["header_name"]]})
    } else if (identical(x = data_type, y = "par")) {
      # v7 to v62 on parameters involves summing over the additional (uniform) sets and adding cgds
      data <- lapply(X = data,
        FUN = function(header) {
          dt <- header[["dt"]]
          nme <- header[["header_name"]]
          v62_colnames <- unlist(x = subset(x = param_conversion,
                                 subset = {is.element(el = v7header,
                                                      set = nme)},
                                 select = v62set))

          drop_val <- colnames(x = dt)[!is.element(el = colnames(x = dt), set = "Value")]
          drop_col <- drop_val[!is.element(el = substring(
            text = drop_val,
            first = 1,
            last = nchar(drop_val) - 1
          ),
          set = v62_colnames)]

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
      # v7 to v62 on base involves changing names, adding cgds, and other op
      data <- lapply(
        X = data,
        FUN = function(header) {
          dt <- header[["dt"]]
          nme <- header[["header_name"]]

          # see if headers differ
          v62_name <- unlist(x = subset(
            x = base_conversion,
            subset = {
              is.element(el = v7header, set = nme)
            },
            select = v62header
          ))

          if (identical(x = nme, y = "VDFB")) {
            cgds <- data.table::copy(purrr::pluck(data, "VDIB", "dt"))
            cgds[, let(PROD_COMMj = "cgds")]
            dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VDFP")) {
            cgds <- data.table::copy(purrr::pluck(data, "VDIP", "dt"))
            cgds[, let(PROD_COMMj = "cgds")]
            dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VMFB")) {
            cgds <- data.table::copy(purrr::pluck(data, "VMIB", "dt"))
            cgds[, let(PROD_COMMj = "cgds")]
            dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (identical(x = nme, y = "VMFP")) {
            cgds <- data.table::copy(x = purrr::pluck(data, "VMIP", "dt"))
            cgds[, let(PROD_COMMj = "cgds")]
            dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          } else if (is.element(el = nme,
                                set = c("EVFB", "EVFP", "FBEP", "FTRV"))) {
            cgds <- data.table::CJ(
              ENDW_COMMi = unique(dt[["ENDW_COMMi"]]),
              PROD_COMMj = "cgds",
              REGr = unique(dt[["REGr"]]),
              Value = 0
            )
            dt <- data.table::rbindlist(l = list(dt, cgds))
          } else if (identical(x = nme, y = "EVOS")) {
            dt <- dt[, .(Value = sum(Value)), by = c("ENDW_COMMi", "REGr")]
          } else if (identical(x = nme, y = "ISEP")) {
            cgds <- data.table::copy(x = dt)
            cgds[, let(PROD_COMMj = "cgds", Value = Value * -1)]
            dt <- data.table::copy(purrr::pluck(data, "CSEP", "dt"))
            dt[, let(Value = Value * -1)]
            dt <- data.table::rbindlist(l = list(dt, cgds), use.names = TRUE)
          }
          return(header)
        }
      )
    }
  }
  return(data)
}
