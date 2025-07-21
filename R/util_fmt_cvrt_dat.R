#' @importFrom purrr pluck
#' @importFrom abind abind
#'
#' @keywords internal
#' @noRd
#' @export
.convert_format.dat <- function(input,
                                target_format,
                                coeff_extract) {

    dat_conversion <- subset(coeff_conversion, data_type == "dat")
    if (target_format %=% "v7.0") {
      # ------------------------------------------------------------------------
      # TVOM has MAKB and MAKS (net OSEP)
      # missing headers (MAKS, MAKB, VDIB, VDIP, VMIP, VMIB, CSEP)
      missing_v7.0 <- unlist(subset(
        dat_conversion,
        is.na(v6.2header),
        v7.0header
      ))

      missing_v7.0 <- lapply(
        missing_v7.0,
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
          arr <- purrr::pluck(input, data_header, "data")
          if (nme %in% c("VDIB", "VDIP", "VMIB", "VMIP")) {
            # get CGDS
            PROD_dim <- which(names(dimnames(arr)) %in% "PROD_COMM")
            arr <- arr[, dim(arr)[PROD_dim], ]
          } else if (nme %=% "CSEP") {
            PROD_dim <- which(names(dimnames(arr)) %in% "PROD_COMM")
            arr <- arr[, -dim(arr)[PROD_dim], , ]
            arr <- arr * -1
          } else if (nme %in% c("MAKB", "MAKS")) {
            sectors <- dimnames(arr)["TRAD_COMM"][[1]]
            regions <- dimnames(arr)["REG"][[1]]

            new_arr <- array(
              data = 0,
              dim = c(length(sectors), length(sectors), length(regions)),
              dimnames = list(TRAD_COMM = sectors, PROD_COMM = sectors, REG = regions)
            )
            # diagonal make
            for (k in 1:length(regions)) {
              for (i in 1:length(sectors)) {
                new_arr[i, i, k] <- arr[i, k]
              }
            }
            if (nme %=% "MAKS") {
              MAK_sub <- purrr::pluck(input, "OSEP", "data")
              sub_arr <- array(
                data = 0,
                dim = c(length(sectors), length(sectors), length(regions)),
                dimnames = list(TRAD_COMM = sectors, PROD_COMM = sectors, REG = regions)
              )
              for (k in 1:length(regions)) {
                for (i in 1:length(sectors)) {
                  sub_arr[i, i, k] <- MAK_sub[i, k]
                }
              }
              arr <- new_arr + sub_arr
            } else {
              arr <- new_arr
            }
          }

          list(
            header = nme,
            data = arr
          )
        }
      )

      input <- lapply(
        input,
        FUN = function(header) {
          arr <- header$data
          nme <- header$header

          if (nme %in% c("EVFA", "VFM", "FBEP", "FTRV", "FBEP", "VDFA", "VDFM", "VIFA", "VIFM")) {
            arr_dimnames <- dimnames(arr)
            PROD_dim <- which(names(arr_dimnames) %in% "PROD_COMM")
            arr_indices <- rep(list(bquote()), length(arr_dimnames))

            # Modify only the PROD_COMM dimension to exclude last element
            PROD_ele <- arr_dimnames[[PROD_dim]]
            arr_indices[[PROD_dim]] <- -length(PROD_ele)
            arr <- do.call("[", c(list(arr), arr_indices))
          } else if (nme %=% "EVOA") {
            # use EVOA with shared out VFM PROD_COMM to recreate EVOS
            VFM_arr <- purrr::pluck(input, "VFM", "data")
            VFM_arr <- VFM_arr[, -length(dimnames(VFM_arr)[[2]]), ]
            VFM_arr_dimnames <- dimnames(VFM_arr)
            VFM_arr_total <- apply(
              VFM_arr,
              MARGIN = c(1, 3),
              FUN = sum
            )

            phi_arr <- array(
              data = 0,
              dim = dim(VFM_arr),
              dimnames = dimnames(VFM_arr)
            )

            new_arr <- array(
              data = 0,
              dim = dim(VFM_arr),
              dimnames = dimnames(VFM_arr)
            )

            # calculate percentage along REG,ENDW and use share with EVOA
            for (i in 1:length(VFM_arr_dimnames$ENDW_COMM)) {
              for (k in 1:length(VFM_arr_dimnames$REG)) {
                phi_arr[i, , k] <- VFM_arr[i, , k] / VFM_arr_total[i, k]
                new_arr[i, , k] <- phi_arr[i, , k] * arr[i, k]
              }
            }

            arr <- new_arr
          } else if (nme %=% "ISEP") {
            # just grab CGDS and *-1
            arr <- arr[, length(dimnames(arr)$PROD_COMM), , ]
            arr <- arr * -1
          }

          header$data <- arr
          return(header)
        }
      )

      input <- lapply(
        input,
        FUN = function(header) {
          .convert_id(
            header = header,
            table = dat_conversion,
            target_format = target_format
          )
        }
      )
      input <- c(input, missing_v7.0)
    } else if (target_format %=% "v6.2") {
      # v7.0 to v6.2 on base involves changing names, adding CGDS, and other op
      # get rid of abind
      input <- lapply(
        input,
        FUN = function(header) {
          arr <- header$data
          arr_dim <- dim(arr)
          arr_dimnames <- dimnames(arr)
          arr_names <- names(arr_dimnames)
          nme <- header$header

          if (nme %in% c("VDFB", "VDFP", "VMFB", "VMFP")) {
            CGDS_header <- switch(nme,
              "VDFB" = "VDIB",
              "VDFP" = "VDIP",
              "VMFB" = "VMIB",
              "VMFP" = "VMIP"
            )
            CGDS_data <- purrr::pluck(input, CGDS_header, "data")
            CGDS_dim <- c(dim(CGDS_data), 1)
            CGDS_dimnames <- c(dimnames(CGDS_data), ACTS = "CGDS")
            a_idx <- match(arr_names, names(CGDS_dimnames), 0)

            CGDS_dimnames <- CGDS_dimnames[a_idx]
            CGDS_dim <- CGDS_dim[a_idx]
            CGDS_data <- array(CGDS_data, dim = CGDS_dim, dimnames = CGDS_dimnames)
            bind_dim <- which(names(CGDS_dimnames) %in% "ACTS")
            arr <- abind::abind(arr, CGDS_data, along = bind_dim)
            names(dimnames(arr)) <- arr_names
          } else if (nme %in% c("EVFB", "EVFP", "FBEP", "FTRV")) {
            ACTS_dim <- which(arr_names %in% "ACTS")
            CGDS_dim <- arr_dim
            CGDS_dim[ACTS_dim] <- 1
            CGDS_dimnames <- arr_dimnames
            CGDS_dimnames[ACTS_dim] <- list(ACTS = "CGDS")
            CGDS_data <- array(0, CGDS_dim, CGDS_dimnames)
            arr <- abind::abind(arr, CGDS_data, along = ACTS_dim)
            names(dimnames(arr)) <- arr_names
          } else if (nme %=% "EVOS") {
            sum_dim <- "ACTS"
            xACTS_dim <- which(!arr_names %in% sum_dim)
            arr <- apply(arr, xACTS_dim, sum)
          } else if (nme %=% "ISEP") {
            CGDS_dim <- c(dim(arr), 1)
            CGDS_dimnames <- c(arr_dimnames, list(ACTS = "CGDS"))
            CGDS_data <- array(arr, CGDS_dim, CGDS_dimnames)
            CGDS_data <- CGDS_data * -1

            arr <- purrr::pluck(input, "CSEP", "data")
            arr <- arr * -1
            arr_names <- names(dimnames(arr))

            a_idx <- match(arr_names, names(CGDS_dimnames))
            CGDS_data <- aperm(CGDS_data, a_idx)
            ACTS_dim <- which(arr_names %in% "ACTS")
            arr <- abind::abind(arr, CGDS_data, along = ACTS_dim)
            names(dimnames(arr)) <- arr_names
          }
          header$data <- arr
          return(header)
        }
      )

      input <- lapply(
        input,
        FUN = function(header) {
          .convert_id(
            header = header,
            table = dat_conversion,
            target_format = target_format
          )
        }
      )
    }

    input <- .rename_headers(input = input,
                             target_format = target_format,
                             coeff_extract = coeff_extract)

    origin_format <- ifelse(target_format %=% "v6.2", "v7.0", "v6.2")
    drop_headers <- with(dat_conversion, {
      origin_col <- paste0(origin_format, "header")
      target_col <- paste0(target_format, "header")
      drop_headers <- get(origin_col)[is.na(get(target_col)) | v6.2header != v7.0header]
    })
    input <- input[!names(input) %in% drop_headers]

  return(input)
}