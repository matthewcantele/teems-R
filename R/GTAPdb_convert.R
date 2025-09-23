#' @keywords internal
#' @noRd
.convert_GTAPdb <- function(i_data) {
  UseMethod(".convert_GTAPdb")
}

#' @method .convert_GTAPdb v6.2
#' @keywords internal
#' @noRd
#' @export
.convert_GTAPdb.v6.2 <- function(i_data) {
  browser()
  missing_headers <- .construct_missing(i_data = i_data)
  i_data <- lapply(i_data, .convert_format)
  
  metadata$data_format <- target_format
  attr(ls_data, "metadata") <- metadata
  
  return(ls_data)
}

#' @method .convert_GTAPdb v7.0
#' @keywords internal
#' @noRd
#' @export
.convert_GTAPdb.v7.0 <- function(i_data) {
  browser()
  i_data <- lapply(i_data, .header2v6)
  # v7.0 to v6.2 on base involves changing names, adding CGDS, and other op
  # get rid of abind
  # r_idx <- match(names(i_data), coeff_conversion$v7.0header)
  # new_header_nmes <- purrr::map2_chr(
  #   r_idx,
  #   names(i_data),
  #   function(id, h) {
  #     if (!is.na(id)) {
  #       coeff_conversion$v6.2header[id]
  #     } else {
  #       h
  #     }
  #   }
  # )
  #   
  # 
  #   id <- grep(header$header, table[[paste0(origin, "header")]])
  # new_name <- table[id, paste0(target_format, "header")]
  
  # if (!new_name %=% character(0) && !is.na(new_name)) {
  #   header$header <- new_name
  # }
  i_data <- lapply(
    i_data,
    FUN = function(arr) {
      browser()
      arr_dim <- dim(arr)
      arr_dimnames <- dimnames(arr)
      arr_names <- names(arr_dimnames)

      if (inherits(arr, c("VDFB", "VDFP", "VMFB", "VMFP"))) {
        CGDS_header <- switch(class(arr)[1],
                              "VDFB" = "VDIB",
                              "VDFP" = "VDIP",
                              "VMFB" = "VMIB",
                              "VMFP" = "VMIP"
        )
        CGDS_data <- i_data[[CGDS_header]]
        CGDS_dim <- c(dim(CGDS_data), 1)
        CGDS_dimnames <- c(dimnames(CGDS_data), ACTS = "CGDS")
        a_idx <- match(arr_names, names(CGDS_dimnames), 0)

        CGDS_dimnames <- CGDS_dimnames[a_idx]
        CGDS_dim <- CGDS_dim[a_idx]
        CGDS_data <- array(CGDS_data, CGDS_dim, CGDS_dimnames)
        bind_dim <- which(names(CGDS_dimnames) %in% "ACTS")
        arr <- abind::abind(arr, CGDS_data, along = bind_dim)
        names(dimnames(arr)) <- arr_names
      } else if (inherits(arr, c("EVFB", "EVFP", "FBEP", "FTRV"))) {
        ACTS_dim <- which(arr_names %in% "ACTS")
        CGDS_dim <- arr_dim
        CGDS_dim[ACTS_dim] <- 1
        CGDS_dimnames <- arr_dimnames
        CGDS_dimnames[ACTS_dim] <- list(ACTS = "CGDS")
        CGDS_data <- array(0, CGDS_dim, CGDS_dimnames)
        arr <- abind::abind(arr, CGDS_data, along = ACTS_dim)
        names(dimnames(arr)) <- arr_names
      } else if (inherits(arr,"EVOS")) {
        sum_dim <- "ACTS"
        xACTS_dim <- which(!arr_names %in% sum_dim)
        arr <- apply(arr, xACTS_dim, sum)
      } else if (inherits(arr, "ISEP")) {
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
      browser()
      return(arr)
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
  
  drop_headers <- subset(
    x = param_conversion,
    subset = {
      is.na(x = v6.2header)
    },
    select = v7.0header
  )[[1]]
  
  input <- input[!is.element(el = names(x = input), set = drop_headers)]
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


  input <- lapply(
    input,
    function(header) {
      .convert_id(
        header = header,
        table = set_conversion,
        target_format = target_format
      )
    }
  )
  
  # add CGDS
  CGDS <- subset(
    set_conversion,
    v6.2header %in% "H9"
  )
  
  CGDS <- list(CGDS_COMM = list(
    header = CGDS$v6.2header,
    type = "character",
    label = CGDS$v6.2description,
    data = "CGDS",
    aggregate = FALSE
  ))
  
  input <- c(input, CGDS)

names(input) <- purrr::map_chr(input, "header")

input <- purrr::map(input,
                    function(s) {
                      r_idx <- match(s$header, set_extract$header)
                      s$label <- purrr::pluck(set_extract, "label", r_idx)
                      return(s)
                    })

keep <- with(set_conversion, get(paste0(target_format, "header")))
input <- input[names(input) %in% keep]
  metadata$data_format <- target_format
  attr(ls_data, "metadata") <- metadata
  
  return(ls_data)
}