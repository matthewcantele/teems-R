#' Parse Coefficients
#'
#' This function parses coefficient data from specified paths, extracts
#' metadata, and constructs a tibble with the metadata. It also checks that
#' outdata coefficients align with tab extract coefficients and reconstructs
#' structured data from ragged outputs.
#'
#'
#' @param paths Character vector. Paths to the coefficient data files.
#' @param coeff_extract Data frame. Extracted coefficient data.
#' @param sets Data frame. Set data.
#' @param chron_yrs Data frame. Time related information.
#' @note Future dev: the main function at bottom works for current models and is
#'   fast but is not generalizable (see "MARG") and is limited to 5 dimensions
#' @note This function needs to be rewritten but the logic behind the peculiar
#'   GEMPack format is not immediately clear
#' @note Rewrite function using this document for guidance:
#'   https://www.copsmodels.com/ftp/gpdoc/rel90/pdf2007/gpd4.pdf Section 6.2
#' @note Split this function
#' @note No fine-tuning until refactored
#'
#' @importFrom purrr pluck map map_chr
#' @importFrom data.table CJ data.table setnames setorderv
#' @return A tibble containing the name, information, and data table (dt) for
#'   each coefficient.
#' @keywords internal
#' @noRd
.parse_coeff <- function(paths,
                       coeff_extract,
                       sets,
                       chron_yrs = NULL) {

  # check paths
  if (!all(sapply(X = paths, FUN = file.exists))) {
    stop("One or more coefficient file paths does not exist")
  }
  # metadata in the first line of each "csv", rest is ragged data
  ls_data <- lapply(X = paths, FUN = readLines)

  list_coeff <- lapply(X = ls_data, FUN = function(data) {
    # remove last NULL
    dat <- data[-1]
    # get name
    name <- purrr::pluck(.x = strsplit(x = data, split = '"'), 1, 2)
    # get dim
    dim <- strsplit(x = trimws(purrr::pluck(.x = strsplit(x = data, split = "Real|Integer"), 1, 1)), " ")
    # get information
    descr <- purrr::pluck(.x = strsplit(x = data, split = '"'), 1, 4)
    list(name, dim, descr, dat)
  })


  # modify these two function to remove numeric indexing
  # construct tibble with metadata
  coeff_tbl <- tibble::tibble(
    name = purrr::map_chr(list_coeff, 1),
    dim = purrr::map(list_coeff, 2),
    information = purrr::map_chr(list_coeff, 3),
    ls_data = purrr::map(list_coeff, 4)
  )

  # filter by specified outdata coefficients
  coeff_extract <- subset(x = coeff_extract, subset = {
    is.element(el = name, set = gsub(pattern = ".csv", replacement = "", x = basename(path = paths)))
  })

  # check that outdata coefficients align with tab extract coefficients
  if (!all(is.element(el = coeff_tbl[["name"]], set = coeff_extract[["name"]]))) {
    stop("One or more coefficients identified from the Tablo extract was not found in the output csvs.")
  }

  # bring integrated set names over from the extract
  r_idx <- match(x = coeff_tbl[["name"]], table = coeff_extract[["name"]])

  coeff_tbl[["setnmes"]] <- coeff_extract[["ls_mixed_idx"]][r_idx]

  # dim length
  coeff_tbl[["l_dim"]] <- sapply(X = coeff_tbl[["dim"]], FUN = function(d) {
    length(x = unlist(x = d))
  })

  # algo to reconstruct structured data from ragged outputs
  # rewrite to remove MARG condition and finite index
  # more helper functions needed to condense
  coeff_tbl$dt <- purrr::pmap(
    .l = list(
      dim = coeff_tbl[["dim"]],
      l_dim = coeff_tbl[["l_dim"]],
      col = coeff_tbl[["setnmes"]],
      ls_dt = coeff_tbl[["ls_data"]]
    ),
    .f = function(dim, l_dim, col, ls_dt) {
      # remove trailing white space
      preDT <- head(x = ls_dt, -1)

      if (dim != "1") {
        plain_col <- tolower(x = substring(text = col, 1, nchar(x = col) - 1))
        r_idx <- match(x = plain_col, table = sets[["setname"]])
        setele <- sets[["elements"]][r_idx]
        columns <- do.call(what = data.table::CJ, args = setele)
        data.table::setnames(x = columns, new = col)
      }

      # nonset condition
      if (identical(x = unlist(x = dim), y = "1")) {
        dt <- data.table::data.table(
          null_set = NA,
          Value = preDT
        )
      } else if (identical(x = l_dim, y = as.integer(x = 1)) && dim != "1") {
        dt <- cbind(columns,
          Value = as.numeric(x = preDT)
        )
      } else if (identical(x = l_dim, y = as.integer(x = 2))) {
        dt <- cbind(columns,
          Value = as.numeric(x = unlist(x = lapply(X = preDT, FUN = strsplit, split = ",")))
        )
      } else if (identical(x = l_dim, y = as.integer(x = 3))) {
        n <- names(x = columns)[c(3, 1, 2)]
        data.table::setorderv(columns, cols = n)
        dt <- cbind(columns,
          Value = as.numeric(x = unlist(x = lapply(X = preDT, FUN = strsplit, split = ",")))
        )
      } else if (identical(x = l_dim, y = as.integer(x = 4)) &&
        !any(grepl(pattern = "MARG", x = colnames(x = columns)))) {
        n <- names(x = columns)[c(4, 3, 1, 2)]
        data.table::setorderv(columns, cols = n)
        dt <- cbind(columns,
          Value = as.numeric(x = unlist(x = lapply(X = preDT, FUN = strsplit, split = ",")))
        )
      } else if (identical(x = l_dim, y = as.integer(x = 4)) &&
        any(grepl(pattern = "MARG", x = colnames(columns)))) {
        n <- names(x = columns)[c(3, 4, 2, 1)]
        data.table::setorderv(columns, cols = n)
        dt <- cbind(columns,
          Value = as.numeric(x = unlist(x = lapply(X = preDT, FUN = strsplit, split = ",")))
        )
      } else if (identical(x = l_dim, y = as.integer(x = 5)) &&
        !any(grepl(pattern = "MARG", x = colnames(x = columns)))) {
        n <- names(x = columns)[c(5, 4, 3, 1, 2)]
        data.table::setorderv(columns, cols = n)
        dt <- cbind(columns,
          Value = as.numeric(x = unlist(x = lapply(X = preDT, FUN = strsplit, split = ",")))
        )
      } else if (identical(x = l_dim, y = as.integer(x = 5)) &&
        any(grepl(pattern = "MARG", x = colnames(x = columns)))) {
        n <- names(x = columns)[c(5, 3, 4, 1, 2)]
        data.table::setorderv(columns, cols = n)
        dt <- cbind(columns,
          Value = as.numeric(x = unlist(x = lapply(X = preDT, FUN = strsplit, split = ",")))
        )
      } else if (l_dim >= as.integer(x = 6)) {
        stop("Extend this algo in aux_coeff_parse.R")
      }
      return(dt)
    }
  )

  names(x = coeff_tbl[["dt"]]) <- coeff_tbl[["name"]]

  # add year information if time_sets != NULL
  if (!is.null(x = chron_yrs)) {
    coeff_tbl[["dt"]] <- lapply(X = coeff_tbl[["dt"]],
                                FUN = .match_year,
                                sets = sets,
                                chron_yrs = chron_yrs)
  }

  # setkey
  lapply(X = coeff_tbl[["dt"]], data.table::setkey)

  coeff_tbl <- subset(x = coeff_tbl, select = c(name, information, dt))
  return(coeff_tbl)
}
