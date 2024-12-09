#' Construct intertemporal parameters
#'
#' This function constructs intertemporal parameter headers.
#'
#' @inheritParams teems_parameters
#' @param sets A data frame or list containing the all sets parsed from the
#'   tablo file.
#'
#' @importFrom tibble remove_rownames as_tibble
#' @importFrom purrr pmap pluck imap_dfr
#' @importFrom data.table CJ data.table setnames setDT copy
#' @return A tibble that integrates the provided parameters with the integer and
#'   non-integer sets, including additional metadata such as lead and index
#'   information.
#' @keywords internal
#' @noRd
.param_int <- function(INID,
                       CPHI,
                       IRAT,
                       KAPP,
                       sets) {

  param_list <- list()
  for (int_param in c("INID", "CPHI", "IRAT", "KAPP")) {
    param_value <- get(x = int_param)
    param_meta <- purrr::pluck(.x = int_params, "v1", int_param)
    if (identical(x = param_meta[["sets"]], y = "null_set")) {
      dt <- data.table::data.table(Value = param_value)
    } else {
      param_sets <- gsub(pattern = ".{1}$",
                         replacement = "",
                         x = param_meta[["sets"]])
      param_ele <- with(data = sets[["elements"]], expr = {
        mget(x = param_sets)
      })

      if (is.numeric(x = param_value)) {
        dt <- do.call(data.table::CJ, args = c(param_ele, Value = param_value))
        data.table::setnames(x = dt,
                             old = param_sets,
                             new = param_meta[["sets"]])
      } else {
        # check any user provided int parameters
        data.table::setDT(x = param_value)
        dt_template <- do.call(data.table::CJ, args = param_ele)
        data.table::setnames(x = dt_template, new = param_meta[["sets"]])
        if (!isTRUE(x = all.equal(target = dt_template,
                  current = param_value[, !"Value"],
                  ignore.row.order = TRUE,
                  check.attributes = FALSE))) {
          stop(paste("The data provided for:",
                     dQuote(int_param),
                     "does not conform to the expected structure."))
        } else {
          dt <- data.table::copy(x = param_value)
        }
      }
    }

    param <- c(param_meta, list(dt = dt))
    param_list[[int_param]] <- param
  }

  header_tbl <- purrr::imap_dfr(
    .x = param_list,
    .f = function(param_entry, param_name) {
      tibble::tibble(
        param = param_name,
        header = param_entry$header,
        information = param_entry$information,
        coeff = param_entry$coeff,
        v_class = param_entry$v_class,
        sets = list(param_entry$sets),
        dt = list(param_entry$dt)
      )
    }
  )

  names(x = header_tbl[["dt"]]) <- header_tbl[["header"]]
  names(x = header_tbl[["information"]]) <- header_tbl[["header"]]
  names(x = header_tbl[["v_class"]]) <- header_tbl[["header"]]

  header_tbl[["lead"]] <- sapply(
    X = header_tbl[["header"]],
    FUN = .construct_lead,
    dat = header_tbl,
    sets = sets
  )

  header_tbl[["idx"]] <- sapply(X = header_tbl[["dt"]], FUN = .get_index)
  return(header_tbl)
}
