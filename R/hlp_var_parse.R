#' @importFrom purrr map2
#' @importFrom data.table data.table CJ setnames fread setkey
#' @importFrom tibble tibble
#' 
#' @keywords internal
#' @noRd
.parse_var <- function(paths,
                       var_extract,
                       vars,
                       sets,
                       chron_yrs = NULL) {

  # now we turn to the var csvs
  # split up the set ids for matching
  vars[["setid"]] <- strsplit(x = vars[["setid"]], split = ",")

  # dim represents a numeric vector corresponding to set id by number of sets
  # 0 indicates no set affiliation (e.g., pfactwld), assigned NA below
  vars[["column_id"]] <- purrr::map2(
    vars[["setid"]],
    vars[["size"]],
    function(x, y) {
      if (y != 0) {
        dim <- x[1:y]
      } else {
        dim <- NA
      }
      return(dim)
    }
  )

  vars[["setnames"]] <- sapply(X = vars[["column_id"]], function(c_id) {
    c_idx <- match(c_id, sets[["r_idx"]])
    set_col <- sets[["setname"]][c_idx]
    return(set_col)
  })

  vars[["elements"]] <- sapply(X = vars[["column_id"]], function(c_id) {
    c_idx <- match(c_id, sets[["r_idx"]])
    set_col <- sets[["elements"]][c_idx]
    return(set_col)
  })

  # cross join to make data.tables (without values or headers)
  vars[["dt"]] <- lapply(X = vars[["elements"]], function(ele) {
    if (is.null(x = unlist(x = ele))) {
      data.table::data.table(null_set = NA)
    } else {
      do.call(what = data.table::CJ, c(ele, sorted = FALSE))
    }
  })

  # give elements column names
  purrr::map2(
    vars[["dt"]],
    vars[["setnames"]],
    function(dt, nmes) {
      if (any(!is.na(x = nmes))) {
        data.table::setnames(dt, toupper(nmes))
      }
      return(dt)
    }
  )

  names(vars[["dt"]]) <- vars[["cofname"]]

  # load data
  dat_dir <- paths[grepl(pattern = "bin_csvs", paths)]
  dat_file <- list.files(path = dat_dir, full.names = TRUE)

  dt <- data.table::fread(input = dat_file,
    header = FALSE,
    skip = 1
  )

  # check that matsize equals nrow for each dt and total n rows equals total data
  if (!all(lapply(X = vars[["dt"]], nrow) == vars[["matsize"]]) ||
    !identical(sum(x = unlist(x = lapply(X = vars[["dt"]], nrow))), nrow(x = dt))) {
    stop("INDICES MISMATCH")
  } else {
    print("No mismatch detected.")
  }

  data.table::setnames(dt, old = c("V1", "V2"), new = c("r_idx", "Value"))

  # bring in variable names by matrix size
  dt[["var"]] <- rep(x = vars[["cofname"]], vars[["matsize"]])
  #dt[, let(var = rep(x = vars[["cofname"]], vars[["matsize"]]))]

  var_out <- sapply(X = vars[["cofname"]], function(nm) {
    sets <- vars[["dt"]][[nm]]
    dt_data <- dt[which(x = var == nm)]
    cbind(sets, dt_data[, -c("r_idx", "var")])
  })

  # check that parsed column names match those within premodel tab extract

  # there is a discrepancy here within the original GTAPv7.0 on cnttechr (REG,TECHTYPE)
  # lax check (column order doesn't matter)
  lax_check <- all(unlist(x = purrr::map2(
    var_extract[["ls_upper_idx"]],
    sapply(X = var_out, colnames),
    function(check, parsed) {
      all(is.element(check, parsed[parsed != "Value"]))
    }
  )))
  if (!lax_check) {
    stop("Lax check:\nOne or more column names in parsed variable data.tables is not present in the tab extract.")
  }

  # this is a strict check so column order matters
  strict_check <- all(unlist(x = purrr::map2(
    var_extract[["ls_upper_idx"]],
    sapply(X = var_out, colnames),
    function(check, parsed) {
      all(check == parsed[parsed != "Value"])
    }
  )))
  if (!strict_check) {
    message('Strict check:\nOne or more column names is either not present (see "Lax check") or in a different order than that of the tab extract.')
  }

  # name check
  if (!all(names(var_out) == tolower(x = var_extract[["name"]]))) {
    stop("Name mismatch in parsed variables with respect to variable extract names")
  }

  # bring back the order-specific subindices
  if (strict_check) {
    purrr::map2(
      var_out,
      var_extract[["ls_mixed_idx"]],
      function(dt, newColnames) {
        data.table::setnames(dt, new = c(newColnames, "Value"))
      }
    )
  }

  # bring over variables information for output
  r_idx <- match(names(x = var_out), tolower(x = var_extract[["name"]]))

  f_var <- tibble::tibble(
    name = var_extract[["name"]][r_idx],
    information = var_extract[["information"]][r_idx],
    dat = var_out
  )

  # add year if intertemporal
  if (!is.null(x = chron_yrs)) {
    f_var[["dat"]] <- lapply(X = f_var[["dat"]],
                            FUN = .match_year,
                            sets = sets,
                            chron_yrs = chron_yrs)
  }
  # set the key
  lapply(X = f_var[["dat"]], data.table::setkey)

  return(f_var)
}
