#' Generate Time Sets for Modeling
#'
#' This function generates a collection of time sets useful for temporal
#' modeling, including chronological years, all time steps, initial and end time
#' steps, and forward-looking time steps. Each set is created with a descriptive
#' header.
#'
#' @inheritParams teems_time
#'
#' @importFrom data.table data.table
#' @return A data frame containing the generated time sets, each as a separate
#'   row, with a column for full sets marked as NA for future binding with
#'   non-integer sets.
#' @keywords internal
#' @noRd
.time_sets <- function(time_steps,
                       t0,
                       reference_year) {

  if (!identical(x = reference_year, y = t0)) {
    stop(paste("The database reference year",
               paste0("(", reference_year, ")"),
               "does not match the chronological year corresponding to the initial timestep",
               paste0("(", t0, ").")))
  }

  n_timestep <- as.numeric(length(x = time_steps) + 1)
  ALLTIME <- .create_header(
    header = "ALLTIME",
    information = "All time steps",
    set_name = "ALLTIME",
    dt = list(data.table::data.table(Value = seq(0, (n_timestep - 1))))
  )

  INITIME <- .create_header(
    header = "INITIME",
    information = "Initial time step",
    set_name = "INITIME",
    dt = list(data.table::data.table(Value = 0))
  )

  ENDTIME <- .create_header(
    header = "ENDTIME",
    information = "End time step",
    set_name = "ENDTIME",
    dt = list(data.table::data.table(Value = n_timestep - 1))
  )

  FWDTIME <- .create_header(
    header = "FWDTIME",
    information = "All time steps except endtime",
    set_name = "FWDTIME",
    dt = list(data.table::data.table(Value = seq(0, (n_timestep - 2))))
  )

  FTRTIME <- .create_header(
    header = "FTRTIME",
    information = "All time steps except initial",
    set_name = "FTRTIME",
    dt = list(data.table::data.table(Value = seq(1, (n_timestep - 1))))
  )

  time_sets <- rbind(ALLTIME, INITIME, ENDTIME, FWDTIME, FTRTIME)

  # add NA for full_sets future rbind with nonint_sets
  time_sets[["full_sets"]] <- NA
  return(time_sets)
}
