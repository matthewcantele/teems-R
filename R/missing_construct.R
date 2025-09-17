.construct_missing <- function(i_data) {
  UseMethod(".construct_missing")
}

#' @method .construct_missing v6.2
#' @export
.construct_missing.v6.2 <- function(i_data) {
  browser()
  param_conversion <- subset(x = coeff_conversion, data_type == "par")
  dat_conversion <- subset(coeff_conversion, data_type == "dat")
  # ------------------------------------------------------------------------
  # TVOM has MAKB and MAKS (net OSEP)
  # missing headers (MAKS, MAKB, VDIB, VDIP, VMIP, VMIB, CSEP)
  missing_headers <- subset(
    coeff_conversion,
    is.na(v6.2header),
    v7.0header,
    1
  )
  
  missing_headers <- lapply(
    missing_headers,
    function(h) {
      data_header <- switch(h,
                            "VDIB" = "VDFM",
                            "VDIP" = "VDFA",
                            "VMIB" = "VIFM",
                            "VMIP" = "VIFA",
                            "CSEP" = "ISEP",
                            "MAKB" = "TVOM",
                            "MAKS" = "TVOM"
      )
      browser()
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
  
  missing_v7.0 <- unlist(x = subset(
    x = param_conversion,
    subset = {
      is.na(x = v6.2header)
    },
    select = v7.0header
  ))
  
  # add missing v7 parameters
  set_ele <- list(
    REG = REG,
    ACTS = ACTS,
    COMM = ACTS,
    MARG = c("atp", "otp", "wtp")
  )
  
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
            header = nme,
            data = arr
          )
        }
      )
    }
  )
  
  names(x = missing_v7.0) <- sapply(
    X = missing_v7.0,
    FUN = function(x) {
      x[["header"]]
    }
  )
}

#' @method .construct_missing v7.0
#' @export
.construct_missing.v7.0 <- function(i_data) {
  # drop_headers <- with(dat_conversion, {
  #   origin_col <- paste0(origin_format, "header")
  #   target_col <- paste0(target_format, "header")
  #   drop_headers <- get(origin_col)[is.na(get(target_col)) | v6.2header != v7.0header]
  # })
  
  # drop_headers <- subset(
  #   x = param_conversion,
  #   subset = {
  #     is.na(x = v6.2header)
  #   },
  #   select = v7.0header
  # )[[1]]
  # 
  # input <- input[!is.element(el = names(x = input), set = drop_headers)]
  return(i_data)
}