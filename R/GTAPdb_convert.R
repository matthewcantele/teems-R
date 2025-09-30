#' @keywords internal
#' @noRd
.convert_GTAPdb <- function(i_data) {
  UseMethod(".convert_GTAPdb")
}

#' @method .convert_GTAPdb v6.2
#' @importFrom purrr map compact list_flatten map_lgl map_chr
#' @keywords internal
#' @noRd
#' @export
.convert_GTAPdb.v6.2 <- function(i_data) {
  
  metadata <- attr(i_data, "metadata")
  missing_par <- subset(
    coeff_conversion,
    is.na(x = v6.2header) & data_type == "par",
    v7.0header,
    1
  )
  missing_par <- purrr::map(missing_par, function(p) {
    class(p) <- c(p, class(p))
    return(p)
  })
  new_v7 <- lapply(c(i_data, missing_par),
                   .v7_new_header,
                   REG = i_data$H1,
                   COMM = i_data$H2,
                   ACTS = i_data$H2,
                   MARG = .o_margin_sectors(),
                   OSEP = i_data$OSEP)
  new_v7 <- purrr::list_flatten(purrr::compact(new_v7))
  names(new_v7) <- purrr::map_chr(new_v7, function(p) {class(p)[1]})
  i_data <- lapply(i_data,
                   .header2v7,
                   i_data = i_data)

  i_data <- c(i_data, new_v7)
  ACTS <- i_data$H2
  class(ACTS)[1:2] <- "ACTS"
  i_data[["ACTS"]] <- ACTS
  keep_set <- subset(
    set_conversion,
    !is.na(v7.0header),
    v7.0header,
    1
  )
  
  i_data <- i_data[purrr::map_lgl(i_data, function(h) {
    if (inherits(h, "set")) {
      inherits(h, keep_set)
    } else {
      TRUE
    }
  })]
  
  names(i_data) <- purrr::map_chr(i_data, function(h) {
    header <- class(h)[1]
  })
  
  metadata$data_format <- "v7.0"
  attr(i_data, "metadata") <- metadata
  class(i_data) <- c("v7.0", class(i_data))
  return(i_data)
}

#' @method .convert_GTAPdb v7.0
#' @importFrom purrr map_lgl map_chr
#' @keywords internal
#' @noRd
#' @export
.convert_GTAPdb.v7.0 <- function(i_data) {
  metadata <- attr(i_data, "metadata")
  drop_par <- subset(
    coeff_conversion,
    is.na(v6.2header) & data_type == "par",
    v7.0header,
    1
  )

  i_data <- i_data[!names(i_data) %in% drop_par]
  i_data <- lapply(i_data, .header2v6, i_data = i_data)
  drop_dat <- subset(
    coeff_conversion,
    is.na(v6.2header) & data_type == "dat",
    v7.0header,
    1
  )

  i_data <- i_data[!names(i_data) %in% drop_dat]

  CGDS_COMM <- structure("CGDS",
    class = c("H9", "CGDS_COMM", "set", "v6.2", "character")
  )

  i_data[["H9"]] <- CGDS_COMM
  keep_set <- subset(
    set_conversion,
    !is.na(v6.2header),
    v6.2header,
    1
  )

  i_data <- i_data[purrr::map_lgl(i_data, function(h) {
    if (inherits(h, "set")) {
      inherits(h, keep_set)
    } else {
      TRUE
    }
  })]
  
  names(i_data) <- purrr::map_chr(i_data, function(h) {
    header <- class(h)[1]
  })

  metadata$data_format <- "v6.2"
  attr(i_data, "metadata") <- metadata
  class(i_data) <- c("v6.2", class(i_data))
  return(i_data)
}