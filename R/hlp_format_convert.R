#' @importFrom purrr pluck
#' @importFrom abind abind
#' 
#' @keywords internal
#' @noRd
.convert_format <- function(ls_array,
                            data_format,
                            data_type) {
  if (identical(x = data_format, y = "v6.2")) {
    if (identical(x = data_type, y = "set")) {
      # change header names and duplicate H2/COMM for ACTS
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

      ACTS <- purrr::pluck(.x = ls_array, "H2")
      ACTS[["header_name"]] <- "ACTS"
      ACTS <- list(ACTS = ACTS)

      ls_array <- c(ls_array, ACTS)
    } else if (identical(x = data_type, y = "par")) {
      # don't see any drawback to using the regions from here rather than bringing forth the prior set object
      REG <- dimnames(x = purrr::pluck(.x = ls_array, "RFLX", "data"))[["REG"]]
      ACTS <- dimnames(x = purrr::pluck(.x = ls_array, "ESBD", "data"))[["TRAD_COMM"]]

      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          arr <- header[["data"]]
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
            if (is.element(el = nme, set = c("ESBT", "ESBV"))) {
              # drop CGDS
              arr <- arr[-length(x = dimnames(x = arr)[[1]])]
            }
            # add region set
            arr_dimnames <- dimnames(x = arr)[[1]]
            arr_names <- names(x = dimnames(x = arr))
            arr <- array(data = arr, dim = c(length(x = arr), length(x = REG)), dimnames = list(arr_dimnames, REG))
            names(x = dimnames(x = arr)) <- c(arr_names, "REG")
            header[["data"]] <- arr
          }

          return(header)
        }
      )
      # add missing v7 parameters
      set_ele <- list(
        REG = REG,
        ACTS = ACTS,
        COMM = ACTS,
        MARG = c("atp", "otp", "wtp")
      )

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
              sets <- with(
                data = set_ele,
                expr = {
                  mget(x = v7.0set[r_idx][[1]])
                }
              )

              if (is.element(el = nme, set = c("ESBG", "ESBS"))) {
                value <- 1
              } else if (identical(x = nme, y = "ETRQ")) {
                value <- -5
              } else if (is.element(el = nme, set = c("ESBC", "ESBQ"))) {
                value <- 0
              }

              arr <- array(
                data = value,
                dim = lapply(X = sets, FUN = length),
                dimnames = sets
              )

              list(
                header_name = nme,
                data = arr
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
      # TVOM has MAKB and MAKS (net OSEP)
      # missing headers (MAKS, MAKB, VDIB, VDIP, VMIP, VMIB, CSEP)
      missing_v7.0 <- unlist(x = subset(
        x = base_conversion,
        subset = {
          is.na(x = v6.2header)
        },
        select = v7.0header
      ))

      missing_v7.0 <- lapply(
        X = missing_v7.0,
        FUN = function(nme) {
          data_header <- switch(EXPR = nme,
            "VDIB" = "VDFM",
            "VDIP" = "VDFA",
            "VMIB" = "VIFM",
            "VMIP" = "VIFA",
            "CSEP" = "ISEP",
            "MAKB" = "TVOM",
            "MAKS" = "TVOM"
          )
          arr <- purrr::pluck(.x = ls_array, data_header, "data")
          if (is.element(el = nme, set = c("VDIB", "VDIP", "VMIB", "VMIP"))) {
            # get CGDS
            PROD_dim <- which(is.element(el = names(x = dimnames(x = arr)), set = "PROD_COMM"))
            arr <- arr[, dim(arr)[PROD_dim], ]
          } else if (identical(x = nme, y = "CSEP")) {
            PROD_dim <- which(is.element(el = names(x = dimnames(x = arr)), set = "PROD_COMM"))
            arr <- arr[, -dim(arr)[PROD_dim], , ]
            arr <- arr * -1
          } else if (is.element(el = nme, set = c("MAKB", "MAKS"))) {
            sectors <- dimnames(x = arr)["TRAD_COMM"][[1]]
            regions <- dimnames(x = arr)["REG"][[1]]

            new_arr <- array(
              data = 0,
              dim = c(length(x = sectors), length(x = sectors), length(x = regions)),
              dimnames = list(TRAD_COMM = sectors, PROD_COMM = sectors, REG = regions)
            )
            # diagonal make
            for (k in 1:length(x = regions)) {
              for (i in 1:length(x = sectors)) {
                new_arr[i, i, k] <- arr[i, k]
              }
            }
            if (identical(x = nme, y = "MAKS")) {
              MAK_sub <- purrr::pluck(.x = ls_array, "OSEP", "data")
              sub_arr <- array(
                data = 0,
                dim = c(length(x = sectors), length(x = sectors), length(x = regions)),
                dimnames = list(TRAD_COMM = sectors, PROD_COMM = sectors, REG = regions)
              )
              for (k in 1:length(x = regions)) {
                for (i in 1:length(x = sectors)) {
                  sub_arr[i, i, k] <- MAK_sub[i, k]
                }
              }
              arr <- new_arr + sub_arr
            } else {
              arr <- new_arr
            }
          }

          list(
            header_name = nme,
            data = arr
          )
        }
      )

      ls_array <- lapply(
        X = ls_array,
        FUN = function(header) {
          arr <- header[["data"]]
          nme <- header[["header_name"]]

          if (is.element(el = nme, set = c("EVFA", "VFM", "FBEP", "FTRV", "FBEP", "VDFA", "VDFM", "VIFA", "VIFM"))) {
            arr_dimnames <- dimnames(x = arr)
            PROD_dim <- which(is.element(el = names(x = arr_dimnames), set = "PROD_COMM"))
            arr_indices <- rep(list(bquote()), length(x = arr_dimnames))

            # Modify only the PROD_COMM dimension to exclude last element
            PROD_ele <- arr_dimnames[[PROD_dim]]
            arr_indices[[PROD_dim]] <- -length(x = PROD_ele)
            arr <- do.call(what = "[", args = c(list(arr), arr_indices))
          } else if (identical(x = nme, y = "EVOA")) {
            # use EVOA with shared out VFM PROD_COMM to recreate EVOS
            VFM_arr <- purrr::pluck(.x = ls_array, "VFM", "data")
            VFM_arr <- VFM_arr[, -length(x = dimnames(x = VFM_arr)[[2]]), ]
            VFM_arr_dimnames <- dimnames(x = VFM_arr)
            VFM_arr_total <- apply(
              X = VFM_arr,
              MARGIN = c(1, 3),
              FUN = sum
            )

            phi_arr <- array(
              data = 0,
              dim = dim(x = VFM_arr),
              dimnames = dimnames(x = VFM_arr)
            )

            new_arr <- array(
              data = 0,
              dim = dim(x = VFM_arr),
              dimnames = dimnames(x = VFM_arr)
            )

            # calculate percentage along REG,ENDW and use share with EVOA
            for (i in 1:length(x = VFM_arr_dimnames[["ENDW_COMM"]])) {
              for (k in 1:length(x = VFM_arr_dimnames[["REG"]])) {
                phi_arr[i, , k] <- VFM_arr[i, , k] / VFM_arr_total[i, k]
                new_arr[i, , k] <- phi_arr[i, , k] * arr[i, k]
              }
            }

            arr <- new_arr
          } else if (identical(x = nme, y = "ISEP")) {
            # just grab CGDS and *-1
            arr <- arr[, length(x = dimnames(x = arr)[["PROD_COMM"]]), , ]
            arr <- arr * -1
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
      CGDS <- subset(
        x = set_conversion,
        subset = {
          is.element(el = v6.2header, set = "H9")
        }
      )

      CGDS <- list(CGDS_COMM = list(
        header_name = CGDS[["v6.2header"]],
        type = "1CFULL",
        information = CGDS[["v6.2description"]],
        data = "CGDS",
        aggregate = FALSE
      ))

      ls_array <- c(ls_array, CGDS)
    } else if (identical(x = data_type, y = "par")) {
      drop_headers <- subset(
        x = param_conversion,
        subset = {
          is.na(x = v6.2header)
        },
        select = v7.0header
      )[[1]]

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
              no = set_table[["v6.2_upper"]][r_idx]
            )

            drop_dim <- cnvrt_dim_names[!is.element(
              el = cnvrt_dim_names,
              set = v6.2_colnames
            )]

            if (!identical(x = drop_dim, y = character(0))) {
              arr <- apply(
                X = arr,
                MARGIN = which(!is.element(
                  el = orig_dim_names,
                  set = drop_dim
                )),
                FUN = unique
              )
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