#' @importFrom purrr pluck pmap transpose simplify
#' @importFrom tibble tibble
#' @importFrom data.table setDT setkey
#' 
#' @keywords internal
#' @noRd
.parse_coeff <- function(paths,
                         coeff_extract,
                         sets,
                         chron_yrs = NULL,
                         call) {
  # check paths
  if (!all(sapply(X = paths, FUN = file.exists))) {
    .cli_action(
      action = "abort",
      msg = "One or more coefficient file paths does not exist.",
      call = call
    )
  }

  # metadata in the first line of each "csv", rest is ragged data
  ls_data <- lapply(X = paths, FUN = readLines)

  list_coeff <- lapply(
    X = ls_data,
    FUN = function(dat) {
      lead <- dat[1]
      # remove last NULL
      dat <- dat[-length(x = dat)]
      # remove lead
      dat <- dat[-1]
      # get name
      name <- purrr::pluck(.x = strsplit(x = lead, split = '"'), 1, 2)
      # get information
      information <- purrr::pluck(.x = strsplit(x = lead, split = '"'), 1, 4)
      # get dim
      dim <- as.integer(x = strsplit(x = trimws(x = sapply(
        X = strsplit(x = lead, split = "Real|Integer"), "[[", 1
      )), "\\s")[[1]])
      
      coeff <- list(name, information, dim, dat)
      return(coeff)
    }
  )

  transposed <- purrr::transpose(.l = list_coeff)

  # Create the tibble
  coeff_tib <- tibble::tibble(
    name = purrr::simplify(.x = transposed[[1]], .type = "character"),
    information = purrr::simplify(.x = transposed[[2]], .type = "character"),
    dim = purrr::simplify(.x = transposed[[3]], .type = "character"),
    ls_data = transposed[[4]]
  )

  # filter by specified outdata coefficients
  coeff_extract <- subset(x = coeff_extract, subset = {
    is.element(
      el = name,
      set = gsub(pattern = ".csv", replacement = "", x = basename(path = paths))
    )
  })

  # check that outdata coefficients align with tab extract coefficients
  if (!all(is.element(el = coeff_extract[["name"]], set = coeff_tib[["name"]]))) {
    .cli_action(
      action = "abort",
      msg = "One or more coefficients identified from the Tablo
                extract was not found in the output csvs.",
      call = call
    )
  }

  # bring mixed set names over from the extract
  r_idx <- match(x = coeff_tib[["name"]], table = coeff_extract[["name"]])
  coeff_tib[["set_nmes"]] <- coeff_extract[["ls_mixed_idx"]][r_idx]

  # if there is a gap in the coefficient declaration like (all, r, REG) we have
  # an error. This should be fixed in the tab parsing section
  coeff_tib[["dat"]] <- purrr::pmap(
    .l = list(
      dimen = coeff_tib[["dim"]],
      col = coeff_tib[["set_nmes"]],
      num_ls = coeff_tib[["ls_data"]]
    ),
    .f = function(dimen, col_nmes, num_ls) {
      dim_length <- length(x = dimen)

      if (!identical(x = dimen, y = 1L)) {
        plain_col <- tolower(x = substring(text = col_nmes, 1, nchar(x = col_nmes) - 1))
        r_idx <- match(x = plain_col, table = sets[["setname"]])
        setele <- sets[["elements"]][r_idx]
        if (is.null(x = setele)) {
          .cli_action(
            action = "abort",
            msg = "It appears that a set isn't found which likely
                      means a space around a coefficient set declaration
                      (e.g., (all, r, REG) instead of (all,r,REG).",
            call = call
          )
        }
      }

      if (identical(x = dim_length, y = 1L)) {
        if (identical(x = dimen, y = 1L)) {
          df <- data.frame(Value = num_ls)
        } else {
          df <- data.frame(setele, Value = as.numeric(x = num_ls))
          colnames(x = df)[1] <- col_nmes
        }
      } else if (identical(x = dim_length, y = 2L)) {
        flat_vec <- as.numeric(x = unlist(x = strsplit(x = num_ls, split = ",")))
        arr <- t(x = array(
          data = flat_vec,
          dim = rev(x = dimen)
        ))
        dimnames(x = arr) <- setele
        names(x = dimnames(x = arr)) <- col_nmes
        df <- array2DF(x = arr)
      } else {
        num_ls <- lapply(X = num_ls, function(r) {
          as.numeric(x = unlist(x = strsplit(x = r, split = ",")))
        })
        null_markers <- sapply(
          X = num_ls,
          FUN = function(l) {
            length(x = l) == 0
          }
        )
        group_ids <- cumsum(x = null_markers)
        split_ls <- split(x = num_ls[!null_markers], f = group_ids[!null_markers])
        ls_mat <- lapply(
          X = split_ls,
          FUN = function(l) {
            t(x = array(data = unlist(x = l), rev(x = dimen[c(1, 2)])))
          }
        )
        arr <- array(data = unlist(x = ls_mat), dim = dimen)
        dimnames(x = arr) <- setele
        names(x = dimnames(x = arr)) <- col_nmes
        df <- array2DF(x = arr)
      }
      data.table::setDT(x = df)
    }
  )

  names(x = coeff_tib[["dat"]]) <- coeff_tib[["name"]]

  # add year information if time_sets != NULL
  if (!is.null(x = chron_yrs)) {
    coeff_tib[["dat"]] <- lapply(
      X = coeff_tib[["dat"]],
      FUN = .match_year,
      sets = sets,
      chron_yrs = chron_yrs
    )
  }

  # setkey
  lapply(X = coeff_tib[["dat"]], data.table::setkey)

  coeff_tib <- subset(x = coeff_tib, select = c(name, information, dat))
  return(coeff_tib)
}
