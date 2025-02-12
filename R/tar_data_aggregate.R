#' Aggregate Data
#'
#'
#' This function aggregates data based on the provided parameters. It performs
#' operations such as set mapping, summing value columns by sets, creating
#' specific sets for lead purposes, and expanding data under certain conditions.
#'
#' @inheritParams .write_sets
#' @param tib_data A tibble. The data to be aggregated produced by
#'   \code{.build_tibble()}.
#'
#' @importFrom purrr pluck
#' @importFrom data.table setnames let rbindlist CJ setcolorder
#' @return A data frame containing the aggregated data.
#' @keywords internal
#' @noRd
.aggregate_data <- function(data_type,
                            tib_data,
                            sets,
                            postagg_header_replace) {

  # sum value columns by sets (condition for whether weighted aggregated occurs)
  # take unique values for others
  if (identical(x = data_type, y = "par")) {
    tib_data[["dt"]] <- lapply(X = tib_data[["dt"]], FUN = function(d) {
      sets <- setdiff(
        x = colnames(x = d),
        y = c("Value", "Weight", "sigma")
      )
      if (all(is.element(el = c("sigma", "Weight"), set = colnames(d)))) {
        d <- d[, lapply(X = .SD, FUN = sum), .SDcols = c("Value", "Weight", "sigma"), by = sets]
        d[, let(Value = sigma / Weight)]
        d[, let(sigma = NULL, Weight = NULL)]
        d[is.nan(x = Value), let(Value = 1)] # for CGDS, return here
      } else {
        # need a more systematic approach here
        # need weights for RWQH and RWQF gdyn headers
        d <- d[, .(Value = mean(Value)), by = setdiff(names(d), "Value")]
      }
    })
  } else if (identical(x = data_type, y = "dat")) {
    tib_data[["dt"]] <- purrr::map2(
      .x = tib_data[["dt"]],
      .y = tib_data[["aggregate"]],
      .f = function(dt, agg) {
        if (agg) {
          sets <- setdiff(x = colnames(x = dt), y = "Value")
          dt <- dt[, .(Value = sum(Value)), by = sets]
        }
        return(dt)
      }
    )
  }

  # add ALLTIME to basedata headers
  if (any(sets[["intertemporal"]])) {
    int_sets <- toupper(x = subset(
      x = sets,
      subset = intertemporal,
      select = name
    )[[1]])
    
    ALLTIMEt <- purrr::pluck(.x = sets, "elements", "ALLTIME")
    
    tib_data[["dt"]] <- lapply(
      X = tib_data[["dt"]],
      FUN = function(dt) {
        stnd_col <- substr(
          x = colnames(x = dt),
          start = 1,
          stop = nchar(colnames(x = dt)) - 1
        )
        if (!any(is.element(el = stnd_col, set = int_sets))) {
          if (!identical(x = colnames(x = dt), y = "Value")) {
          set_col <- setdiff(x = colnames(x = dt), y = "Value")
          dt <- dt[, .(ALLTIMEt, Value), by = set_col]
          }
        }
        return(dt)
      }
    )
  }

  # check and swap in any user-provided headers
  if (!is.null(x = postagg_header_replace)) {

    if (any(!sapply(X = postagg_header_replace, file.exists))) {
      stop("One or more files for user-provided headers does not exist.")
    }

    for (header in names(x = postagg_header_replace)) {
      # Read the CSV file into a dt
      custom_header <- data.table::fread(input = postagg_header_replace[[header]])
      header_name <- names(x = custom_header)

      if (!is.element(el = "Value", set = colnames(x = custom_header))) {
        stop(paste(dQuote(x = "Value"),
                   "column missing from the post-agg header:",
                   header_name))
      }

      # default values
      header_template <- purrr::pluck(.x = tib_data, "dt", header)
      template_col <- colnames(x = header_template)
browser()
      # check that datasets are equal
      if (!isTRUE(x = all.equal(
        current = custom_header[, !"Value"],
        target = header_template[, !"Value"],
        ignore.row.order = TRUE,
        ignore.col.order = TRUE
      ))) {
        stop(paste("One or more columns and/or rows in the new post-agg header data for:",
                   header_name,
                   "is inconsistent or missing."))
      }

      # rearrange if necessary
      if (!identical(x = template_col, y = colnames(x = custom_header))) {
        data.table::setcolorder(x = custom_header, neworder = template_col)
      }

      # swap in
      tib_data[["dt"]][[header]] <- custom_header
    }
  }

  # may be some redundancy here with col ordering and function above
  tib_data[["lead"]] <- sapply(
    X = tib_data[["header"]],
    FUN = .construct_lead,
    dat = tib_data,
    sets = sets
  )

  # algo for WriteData()
  tib_data[["idx"]] <- sapply(X = tib_data[["dt"]], FUN = .get_index)

  # sort headers
  tib_data <- tib_data[order(tib_data[["header"]]), ]

  return(tib_data)
}
