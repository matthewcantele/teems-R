#' @importFrom purrr pluck
#' @importFrom abind abind
#'
#' @keywords internal
#' @noRd
#' @method .convert_format dat_v6.2
#' @export
.convert_format.dat_v6.2 <- function(input) {
  browser()
  dat_conversion <- subset(coeff_conversion, data_type == "dat")
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
    browser()
  #   input <- lapply(
  #     input,
  #     FUN = function(header) {
  #       .convert_id(
  #         header = header,
  #         table = dat_conversion,
  #         target_format = target_format
  #       )
  #     }
  #   )
  # }
  # 
  # input <- .rename_headers(input = input,
  #                          target_format = target_format,
  #                          coeff_extract = coeff_extract)
  # 
  # origin_format <- ifelse(target_format %=% "v6.2", "v7.0", "v6.2")
  # drop_headers <- with(dat_conversion, {
  #   origin_col <- paste0(origin_format, "header")
  #   target_col <- paste0(target_format, "header")
  #   drop_headers <- get(origin_col)[is.na(get(target_col)) | v6.2header != v7.0header]
  # })
  # input <- input[!names(input) %in% drop_headers]
  
  return(input)
}