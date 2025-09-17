#' @importFrom purrr pluck
#' @importFrom abind abind
#'
#' @keywords internal
#' @noRd
#' @export
.convert_format.par <- function(input,
                                target_format,
                                coeff_extract) {
  browse()
    # param_conversion <- subset(x = coeff_conversion, data_type == "par")
    if (identical(x = target_format, y = "v7.0")) {
      # don't see any drawback to using the regions from here rather than bringing forth the prior set object
      REG <- dimnames(x = purrr::pluck(.x = input, "RFLX", "data"))[["REG"]]
      ACTS <- dimnames(x = purrr::pluck(.x = input, "ESBD", "data"))[["TRAD_COMM"]]

      input <- lapply(
        X = input,
        FUN = function(header) {
          arr <- header[["data"]]
          nme <- header[["header"]]
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
      # # add missing v7 parameters
      # set_ele <- list(
      #   REG = REG,
      #   ACTS = ACTS,
      #   COMM = ACTS,
      #   MARG = c("atp", "otp", "wtp")
      # )

      # missing_v7.0 <- unlist(x = subset(
      #   x = param_conversion,
      #   subset = {
      #     is.na(x = v6.2header)
      #   },
      #   select = v7.0header
      # ))
      # 
      # # should pass in via fun rather than parent.frame
      # missing_v7.0 <- with(
      #   data = param_conversion,
      #   expr = {
      #     lapply(
      #       X = missing_v7.0,
      #       FUN = function(nme) {
      #         r_idx <- grep(pattern = nme, x = v7.0header)
      #         sets <- with(
      #           data = set_ele,
      #           expr = {
      #             mget(x = v7.0set[r_idx][[1]])
      #           }
      #         )
      # 
      #         if (is.element(el = nme, set = c("ESBG", "ESBS"))) {
      #           value <- 1
      #         } else if (identical(x = nme, y = "ETRQ")) {
      #           value <- -5
      #         } else if (is.element(el = nme, set = c("ESBC", "ESBQ"))) {
      #           value <- 0
      #         }
      # 
      #         arr <- array(
      #           data = value,
      #           dim = lapply(X = sets, FUN = length),
      #           dimnames = sets
      #         )
      # 
      #         list(
      #           header = nme,
      #           data = arr
      #         )
      #       }
      #     )
      #   }
      # )
      # 
      # names(x = missing_v7.0) <- sapply(
      #   X = missing_v7.0,
      #   FUN = function(x) {
      #     x[["header"]]
      #   }
      # )
      input <- c(input, missing_v7.0)
    } else if (identical(x = target_format, y = "v6.2")) {
      # drop_headers <- subset(
      #   x = param_conversion,
      #   subset = {
      #     is.na(x = v6.2header)
      #   },
      #   select = v7.0header
      # )[[1]]
      # 
      # input <- input[!is.element(el = names(x = input), set = drop_headers)]
      # v7.0 to v6.2 on parameters involves summing over the additional (uniform) sets and adding zcgds
      input <- lapply(
        X = input,
        FUN = function(header) {
          arr <- header[["data"]]
          orig_dim_names <- names(x = dimnames(x = arr))
          nme <- header[["header"]]
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
            
            if (nme %=% "ETRE") {
              arr <- arr[c("Land", "NatlRes")] 
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
    }

    input <- .rename_headers(input = input,
                             target_format = target_format,
                             coeff_extract = coeff_extract)
    
    origin_format <- ifelse(target_format %=% "v6.2", "v7.0", "v6.2")
    drop_headers <- with(param_conversion, {
      origin_col <- paste0(origin_format, "header")
      target_col <- paste0(target_format, "header")
      drop_headers <- get(origin_col)[is.na(get(target_col)) | v6.2header != v7.0header]
    })
    input <- input[!names(input) %in% drop_headers]
    
  return(input)
}