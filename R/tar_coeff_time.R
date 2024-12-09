#' Generate Time Coefficients for Modeling
#'
#' This function generates time coefficients for modeling by reconstructing
#' integer set arguments from the "time_sets" target. It calculates the number
#' of timesteps and the step intervals, and creates headers for the number of
#' timesteps (NTSP) and actual years (AYRS).
#'
#' @inheritParams teems_time
#'
#' @param sets A data frame or list containing the intertemporal sets used to
#'   calculate time coefficients.
#' @param base_year The base year from which the chronological years are
#'   calculated.
#'
#' @importFrom purrr pluck
#' @importFrom data.table data.table
#' @return A data frame containing headers for the number of timesteps and
#'   actual years, along with additional metadata such as lead and index
#'   information.
#' @keywords internal
#' @noRd
.time_coeff <- function(sets,
                        time_steps,
                        base_year) {

  n_timestep <- as.numeric(length(x = time_steps) + 1)
  NTSP <- .create_header(
    header = "NTSP",
    information = "Number of timesteps",
    coeff = "NTSP",
    v_class = "time",
    dt = list(data.table::data.table(Value = n_timestep))
  )

  AYRS <- .create_header(
    header = "AYRS",
    information = "Actual years",
    coeff = "AYRS",
    v_class = "time",
    dt = list(data.table::data.table(
      ALLTIMEt = seq(0, (n_timestep - 1)),
      Value = c(0, cumsum(x = time_steps))
    ))
  )

  CYRS <- .create_header(
    header = "CYRS",
    information = "Chronological years",
    coeff = "CYRS",
    v_class = "time",
    dt = list(data.table::data.table(
      ALLTIMEt = seq(0, (n_timestep - 1)),
      Value = base_year + c(0, cumsum(x = time_steps))
    ))
  )

  time_headers <- rbind(NTSP, AYRS, CYRS)

  # lead
  time_headers[["lead"]] <- sapply(
    X = time_headers[["header"]],
    FUN = .construct_lead,
    dat = time_headers,
    sets = sets,
    numeric_type = "Integer"
  )

  # algo for writer
  time_headers[["idx"]] <- sapply(X = time_headers[["dt"]], FUN = .get_index)

  time_headers <- time_headers[order(time_headers[["header"]]), ]
  return(time_headers)
}
