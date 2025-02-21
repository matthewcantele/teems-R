#' @importFrom purrr pluck
#' @importFrom abind abind
#' @importFrom data.table CJ copy rbindlist setnames merge.data.table
#' 
#' @keywords internal
#' @noRd
.convert_format <- function(ls_array,
                            data_format,
                            data_type) {

  if (identical(x = data_format, y = "v6.2")) {
    if (identical(x = data_type, y = "set")) {
      browser()
      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          .convert_id(
            header = header,
            table = set_conversion,
            origin = data_format,
            colname = TRUE
          )
        }
      )

      ACTS <- purrr::pluck(.x = ls_array, "H2")
      ACTS[["header_name"]] <- "ACTS"
      colnames(ACTS[["dt"]]) <- "ACTS"
      ACTS <- list(ACTS = ACTS)

      ls_array <- c(ls_array, ACTS)
    } else if (identical(x = data_type, y = "par")) {
      browser()
      # don't see any drawback to using the regions from here rather than bringing forth the prior set object
      REGr <- purrr::pluck(.x = ls_array, "RFLX", "dt", "REGr")
      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          dt <- header[["dt"]]
          nme <- header[["header_name"]]
          v7.0_colnames <- unlist(x = subset(
            x = param_conversion,
            subset = {
              is.element(
                el = v6.2header,
                set = nme
              )
            },
            select = v7.0set
          ))
          if (is.element(el = nme, set = c("ESBD", "ESBM", "ETRE", "ESBT", "ESBV"))) {
            set_col <- setdiff(x = colnames(x = dt), y = "Value")
            new_dt <- dt[, .(REGr, Value), by = set_col]
            if (is.element(el = nme, set = c("ESBT", "ESBV"))) {
              new_dt <- new_dt[ACTSa != "zcgds", ]
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
      ACTS <- unique(purrr::pluck(.x = ls_array, "ESBV", "dt", "ACTSa"))
      COMM <- ACTS
      MARG <- c("atp", "otp", "wtp")

      missing_v7.0 <- unlist(x = subset(
        x = param_conversion,
        subset = {
          is.na(x = v6.2header)
        },
        select = v7.0header
      ))

      # should pass in via fun rather than parent.frame
      missing_v7.0 <- with(
        data = param_conversion,
        expr = {
          lapply(
            X = missing_v7.0,
            FUN = function(nme) {
              r_idx <- grep(pattern = nme, x = v7.0header)
              sets <- v7.0set[r_idx][[1]]
              # descr <- v7.0description[r_idx][[1]]
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

              dt <- do.call(
                what = data.table::CJ,
                args = c(set_ele, Value = value)
              )

              list(
                header_name = nme,
                # information = descr,
                dt = dt
              )
            }
          )
        }
      )

      names(x = missing_v7.0) <- sapply(
        X = missing_v7.0,
        FUN = function(x) {
          x[["header_name"]]
        }
      )
      ls_array <- c(ls_array, missing_v7.0)
    } else if (identical(x = data_type, y = "base")) {
      browser()
      # TVOM has MAKB and MAKS (net OSEP)
      # missing headers (MAKS, MAKB, VDIB, VDIP, VMIP, VMIB, CSEP)
      missing_v7.0 <- unlist(x = subset(
        x = base_conversion,
        subset = {
          is.na(x = v6.2header)
        },
        select = v7.0header
      ))

      missing_v7.0 <- with(
        data = base_conversion,
        expr = {
          lapply(
            X = missing_v7.0,
            FUN = function(nme) {
              if (identical(x = nme, y = "VDIB")) {
                dt <- data.table::copy(x = purrr::pluck(.x = ls_array, "VDFM", "dt"))
                dt <- dt[ACTSa == "zcgds", !("ACTSa"), with = FALSE]
              } else if (identical(x = nme, y = "VDIP")) {
                dt <- data.table::copy(x = purrr::pluck(.x = ls_array, "VDFA", "dt"))
                dt <- dt[ACTSa == "zcgds", !("ACTSa"), with = FALSE]
              } else if (identical(x = nme, y = "VMIB")) {
                dt <- data.table::copy(x = purrr::pluck(.x = ls_array, "VIFM", "dt"))
                dt <- dt[ACTSa == "zcgds", !("ACTSa"), with = FALSE]
              } else if (identical(x = nme, y = "VMIP")) {
                dt <- data.table::copy(x = purrr::pluck(.x = ls_array, "VIFA", "dt"))
                dt <- dt[ACTSa == "zcgds", !("ACTSa"), with = FALSE]
              } else if (identical(x = nme, y = "CSEP")) {
                dt <- data.table::copy(x = purrr::pluck(.x = ls_array, "ISEP", "dt"))
                dt <- dt[ACTSa != "zcgds"]
                dt[, Value := Value * -1]
              } else if (is.element(el = nme, set = c("MAKB", "MAKS"))) {
                dt <- data.table::copy(x = purrr::pluck(.x = ls_array, "TVOM", "dt"))
                dt[, ACTSa := COMMc]
                sectors <- unique(x = dt[["COMMc"]])
                null_dt <- data.table::CJ(
                  COMMc = sectors,
                  ACTSa = sectors,
                  REGr = unique(x = dt[["REGr"]]),
                  Value = 0
                )
                null_dt <- null_dt[COMMc != ACTSa, ]
                dt <- data.table::rbindlist(l = list(null_dt, dt), use.names = TRUE)
                if (identical(x = nme, y = "MAKS")) {
                  OSEP <- data.table::copy(x = purrr::pluck(.x = ls_array, "OSEP", "dt"))
                  OSEP[, ACTSa := COMMc]
                  data.table::setnames(x = OSEP, old = "Value", new = "OSEP")
                  dt <- data.table::merge.data.table(
                    x = dt,
                    y = OSEP,
                    by = c("COMMc", "ACTSa", "REGr"),
                    all.x = TRUE
                  )
                  dt[, Value := Value + OSEP]
                  dt[is.na(x = Value), Value := 0]
                  dt[, OSEP := NULL]
                }
              }

              list(
                header_name = nme,
                dt = dt
              )
            }
          )
        }
      )
      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          dt <- header[["dt"]]
          nme <- header[["header_name"]]

          if (is.element(el = nme, set = c(
            "EVFA",
            "VFM",
            "FBEP",
            "FTRV",
            "FBEP",
            "VDFA",
            "VDFM",
            "VIFA",
            "VIFM"
          ))) {
            new_dt <- dt[ACTSa != "zcgds"]
          } else if (identical(x = nme, y = "EVOA")) {
            # use EVOA with shared out VFM PROD_COMM to recreate EVOS
            VFM <- data.table::copy(purrr::pluck(.x = ls_array, "VFM", "dt"))
            col_names <- colnames(x = VFM)
            VFM <- VFM[ACTSa != "zcgds", ]
            VFM_totals <- VFM[, .(total = sum(Value)), by = c("ENDWe", "REGr")]
            VFM <- data.table::merge.data.table(
              x = VFM,
              y = VFM_totals,
              by = c("ENDWe", "REGr")
            )
            VFM[, phi := Value / total]
            dt <- data.table::merge.data.table(
              x = dt,
              y = VFM[, c("ENDWe", "REGr", "ACTSa", "phi")]
            )
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

      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          .convert_id(
            header = header,
            table = base_conversion,
            origin = data_format
          )
        }
      )
      ls_array <- c(ls_array, missing_v7.0)
    }
  } else if (identical(x = data_format, y = "v7.0")) {
    if (identical(x = data_type, y = "set")) {
      # v7.0 to v6.2 on sets involves changing set names
      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          .convert_id(
            header = header,
            table = set_conversion,
            origin = data_format
          )
        }
      )

      # add CGDS
      CGDS <- subset(x = set_conversion,
                     subset = {is.element(el = v6.2header, set = "H9")})
      
      CGDS <- list(CGDS_COMM = list(
        header_name = CGDS[["v6.2header"]],
        type = "1CFULL",
        information = CGDS[["v6.2description"]],
        data = "CGDS",
        aggregate = FALSE
      ))
      
      ls_array <- c(ls_array, CGDS)
    } else if (identical(x = data_type, y = "par")) {
      drop_headers <- subset(x = param_conversion,
                             subset = {is.na(x = v6.2header)},
                             select = v7.0header)[[1]]
      
      ls_array <- ls_array[!is.element(el = names(x = ls_array), set = drop_headers)]
      # v7.0 to v6.2 on parameters involves summing over the additional (uniform) sets and adding zcgds
      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          arr <- header[["data"]]
          orig_dim_names <- names(x = dimnames(x = arr))
          nme <- header[["header_name"]]
          v6.2_colnames <- unlist(x = subset(
            x = param_conversion,
            subset = {
              is.element(
                el = v7.0header,
                set = nme
              )
            },
            select = v6.2set
          ))

          if (!anyNA(x = v6.2_colnames) && !is.null(x = v6.2_colnames)) {
          r_idx <- match(x = orig_dim_names, table = set_table[["v7.0_upper"]])
          cnvrt_dim_names <- ifelse(test = is.na(x = r_idx),
                                    yes = orig_dim_names,
                                    no = set_table[["v6.2_upper"]][r_idx])
          
          drop_dim <- cnvrt_dim_names[!is.element(el = cnvrt_dim_names,
                                                  set = v6.2_colnames)]

          if (!identical(x = drop_dim, y = character(0))) {
          arr <- apply(X = arr,
                       MARGIN = which(!is.element(el = orig_dim_names,
                                                 set = drop_dim)),
                       FUN = unique)
          }
          
          # CGDS additions
          if (identical(x = nme, y = "ESBT")) {
            CGDS <- array(data = 0, dimnames = list("CGDS"))
            arr <- c(arr, CGDS)
          }

          if (identical(x = nme, y = "ESBV")) {
            CGDS <- array(data = 1, dimnames = list("CGDS"))
            arr <- c(arr, CGDS)
          }

          if (!is.array(x = arr)) {
            arr <- as.array(x = arr)
          }

          names(x = dimnames(x = arr)) <- v6.2_colnames
          }
          header[["data"]] <- arr
          return(header)
        }
      )
    } else if (identical(x = data_type, y = "base")) {
      # v7.0 to v6.2 on base involves changing names, adding CGDS, and other op
      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          arr <- header[["data"]]
          arr_dim <- dim(x = arr)
          arr_dimnames <- dimnames(x = arr)
          arr_names <- names(x = arr_dimnames)
          nme <- header[["header_name"]]
          
          if (is.element(el = nme, set = c("VDFB", "VDFP", "VMFB", "VMFP"))) {
            CGDS_header <- switch(EXPR = nme,
              "VDFB" = "VDIB",
              "VDFP" = "VDIP",
              "VMFB" = "VMIB",
              "VMFP" = "VMIP"
            )
            CGDS_data <- purrr::pluck(.x = ls_array, CGDS_header, "data")
            CGDS_dim <- c(dim(x = CGDS_data), 1)
            CGDS_dimnames <- c(dimnames(x = CGDS_data), ACTS = "CGDS")
            a_idx <- match(x = arr_names, names(x = CGDS_dimnames), nomatch = 0)

            CGDS_dimnames <- CGDS_dimnames[a_idx]
            CGDS_dim <- CGDS_dim[a_idx]
            CGDS_data <- array(CGDS_data, dim = CGDS_dim, dimnames = CGDS_dimnames)
            bind_dim <- which(is.element(el = names(x = CGDS_dimnames), set = "ACTS"))
            arr <- abind::abind(arr, CGDS_data, along = bind_dim)
            names(x = dimnames(x = arr)) <- arr_names
          } else if (is.element(el = nme, set = c("EVFB", "EVFP", "FBEP", "FTRV"))) {
            ACTS_dim <- which(is.element(el = arr_names, set = "ACTS"))
            CGDS_dim <- arr_dim
            CGDS_dim[ACTS_dim] <- 1
            CGDS_dimnames <- arr_dimnames
            CGDS_dimnames[ACTS_dim] <- list(ACTS = "CGDS")
            CGDS_data <- array(data = 0, dim = CGDS_dim, dimnames = CGDS_dimnames)
            arr <- abind::abind(arr, CGDS_data, along = ACTS_dim)
            names(x = dimnames(x = arr)) <- arr_names
          } else if (identical(x = nme, y = "EVOS")) {
            sum_dim <- "ACTS"
            xACTS_dim <- which(!is.element(el = arr_names, set = sum_dim))
            arr <- apply(X = arr, MARGIN = xACTS_dim, FUN = sum)
          } else if (identical(x = nme, y = "ISEP")) {
            CGDS_dim <- c(dim(x = arr), 1)
            CGDS_dimnames <- c(arr_dimnames, list(ACTS = "CGDS"))
            CGDS_data <- array(arr, dim = CGDS_dim, dimnames = CGDS_dimnames)
            CGDS_data <- CGDS_data * -1
            
            arr <- purrr::pluck(ls_array, "CSEP", "data")
            arr <- arr * -1
            arr_names <- names(x = dimnames(x = arr))
            
            a_idx <- match(arr_names, names(CGDS_dimnames))
            CGDS_data <- aperm(a = CGDS_data, perm = a_idx)
            ACTS_dim <- which(is.element(el = arr_names, set = "ACTS"))
            arr <- abind::abind(arr, CGDS_data, along = ACTS_dim)
            names(x = dimnames(x = arr)) <- arr_names
          }
          header[["data"]] <- arr
          return(header)
        }
      )
      
      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          .convert_id(
            header = header,
            table = base_conversion,
            origin = data_format
          )
        }
      )
    }
  }

  names(x = ls_array) <- sapply(
    X = ls_array,
    FUN = function(x) {
      x[["header_name"]]
    }
  )

  return(ls_array)
}
