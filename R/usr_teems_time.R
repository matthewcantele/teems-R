#' Load time specifications
#'
#' @description `teems_time()` loads time related data for non-static
#'   model runs. This includes parameters specific to dynamic runs as
#'   well as the number of timesteps and inferred intrastep intervals.
#'   The output of this function is a required value for the
#'   [`teems_deploy()`] `"time_config"` argument when temporal
#' dynamics are not static (i.e., dynamic).
#'
#' Learn more about this function including Tablo file limitations in
#' `vignette("something")`
#'
#' @param time_steps Integer vector of variable length (default is
#'   `NULL`). `"time_steps"` are inputted as the desired chronological
#'   years of steps including initial reference year.
#' @param CPHI Numeric of length 1 or path to a csv with value (Value)
#'   by region (REGr) and timestep (ALLTIMEt) (default is `0.1`). The
#'   investment adjustment coefficient shows the ratio of investment
#'   (at a $1 cost basis) necessary to gain the equivalent increase in
#'   capital stock. For example, the default value .1 would indicate
#'   that for every $1.10 in investment, capital stock increases by
#'   $1. It represents the installment cost in investment process. The
#'   adjustment cost function is typically a convex function. The
#'   investment adjustment rate reflects how quickly the level of
#'   investment adjusts in response to economic changes or new
#'   information.
#' @param KAPPA Numeric of length 1 or path to a csv with value
#'   (Value) by region (REGr) and timestep (ALLTIMEt) (default is
#'   `0.05`). The depreciation rate is the rate at which a physical
#'   capital loses its value over time due to wear and tear,
#'   obsolescence, or usage. It is a critical measure in accounting
#'   and economics, as it determines the reduction in value of fixed
#'   assets like machinery, equipment, and buildings. A higher
#'   depreciation rate indicates a faster decline in an asset's
#'   productive value.
#' @param LRORG Numeric of length 1 or path to a csv with value
#'   (Value) by timestep (ALLTIMEt) (default is `0.05`). The global
#'   interest rate or rate of return refers to the average rate at
#'   which investments yield returns across the global economy. This
#'   rate is influenced by various factors, including international
#'   demand for capital, central bank policies, inflation rates, and
#'   global economic conditions.
#' @param INIDELTA Logical of length 1 (default is `TRUE`). INIDELTA
#'   determines the intertemporal initial condition switching
#'   mechanism. When TRUE, VKB and the value of ps("capital",r,t) will
#'   be adjusted so the current base year data will become steady
#'   state of the economy. When FALSE, VKB will be read from data and
#'   the value of ps("capital",r,t) will be calculated by definition.
#'   The values of the auxiliary variables - LCAPHAT(r,t) and
#'   LMUHAT(r,t) - should be one. If they are not 1 then shock them to
#'   1 to get the baseline scenario.
#' @param interval_switch Logical length 1 (default is `FALSE`).
#'   Switch controlling interpretation of `"time_steps"` input. When
#'   `TRUE`, each element of `"time_steps"` is interpreted as an
#'   interval between 2 time steps.
#'
#' @return A list of time configuration options.
#'
#' @details `teems_time()` return values have no purpose used in
#'   isolation and are rather combined with user inputs in other
#'   `teems` package functions within [`teems_deploy()`] to produce a
#'   path-dependent pipeline resulting in solver-ready input files for
#'   [`teems_solve()`].
#'
#' @seealso [`teems_deploy()`] for loading the output of this
#'   function.
#'
#' @examples
#' # See `vignette("something")` for examples and explanation
#'
#' temp_dir <- tools::R_user_dir(package = "teems", "cache")
#' if (!dir.exists(temp_dir)) {
#'   dir.create(temp_dir)
#' }
#' file.copy(from = c(teems_example(path = "user_CPHI.csv"),
#'                    teems_example(path = "user_LRORG.csv")),
#'           to = temp_dir)
#' CPHI_path <- file.path(temp_dir, "user_CPHI.csv")
#' LRORG_path <- file.path(temp_dir, "user_LRORG.csv")
#'
#' # Time steps can be loaded by directly specifying the chronological
#' # years to be used as steps (including the initial base year). Note
#' # that chronological years will be converted to timestep intervals
#' # (see `"interval_switch"`) and no check will be conducted to
#' # verify that the initial year matches chosen reference data.
#' time_config <- teems_time(time_steps = c(2011, 2012, 2015, 2018))
#'
#' # Time steps can also be loaded as a vector of integer values
#' # representing intervals. The following formulations result in the
#' # same time steps as above:
#' time_config <- teems_time(time_steps = c(1, 3, 3),
#'                           interval_switch = TRUE)
#'
#' time_config <- teems_time(time_steps = c(1, rep(3, 2)),
#'                           interval_switch = TRUE)
#'
#' # Intertemporal parameters can be set uniformly or provided as a
#' # file input. Set checks will be conducted if inputted as a file.
#' read.csv(CPHI_path)
#' read.csv(LRORG_path)
#'
#' time_config <- teems_time(time_steps = c(2011, 2012, 2015, 2018),
#'                           CPHI = CPHI_path,
#'                           LRORG = LRORG_path)
#'
#'
#' unlink(x = temp_dir, recursive = TRUE)
#'
#' @export
teems_time <- function(time_steps,
                       CPHI = 0.1,
                       KAPPA = 0.05,
                       LRORG = 0.05,
                       INIDELTA = TRUE,
                       interval_switch = FALSE)
{
if (!interval_switch) {
  t0 <- time_steps[1]
  time_steps <- diff(x = time_steps)
} else {
  t0 <- NA
}
stopifnot(is.numeric(x = time_steps))
stopifnot(is.logical(x = INIDELTA))
stopifnot(is.logical(x = interval_switch))
if (INIDELTA) {
  INIDELTA <- 1L
  } else {
  INIDELTA <- 0L
}
n_timestep <- as.numeric(length(x = time_steps) + 1)
if (grepl(pattern = "\\.csv", x = CPHI)) {
  if (!file.exists(CPHI)) {
    stop(paste("Filepath for",
               dQuote(x = "CPHI"),
               "is not found."))
  } else {
    CPHI_file <- read.csv(file = CPHI)
    if (!all(is.element(el = colnames(x = CPHI_file),
                        set = c("REGr", "ALLTIMEt", "Value")))) {
      stop(paste("The user-provided intertemporal parameter file for",
                 dQuote(x = "CPHI"),
                 "does not have one or more of the required columns: REGr, ALLTIMEt, Value"))
    } else {
      implied_ALLTIMEt <- as.integer(x = sort(x = unique(x = CPHI_file[["ALLTIMEt"]])))
      correct_ALLTIMEt <- seq(from = 0, to = n_timestep - 1)
      if (!identical(x = implied_ALLTIMEt, y = correct_ALLTIMEt)) {
        stop(cat("The user-provided ALLTIMEt set steps for",
                 dQuote(x = "CPHI"),
                 "are not consistent with the 'n_timestep' value of:",
                 n_timestep,
                 "\nThere should be a",
                 dQuote(x = "Value"),
                 "entry for every REGr by",
                 toString(x = correct_ALLTIMEt)))
      }
    }
  }
}
if (grepl(pattern = "\\.csv", x = LRORG)) {
  if (!file.exists(LRORG)) {
    stop(paste("Filepath for",
               dQuote(x = "LRORG"),
               "is not found."))
  } else {
    LRORG_file <- read.csv(file = LRORG)
    if (!all(is.element(el = colnames(x = LRORG_file),
                        set = c("ALLTIMEt", "Value")))) {
      stop(paste("The user-provided intertemporal parameter file for",
                 dQuote(x = "LRORG"),
                 "does not have one or more of the required columns: ALLTIMEt, Value"))
    } else {
      implied_ALLTIMEt <- as.integer(x = sort(x = unique(x = LRORG_file[["ALLTIMEt"]])))
      correct_ALLTIMEt <- seq(from = 0, to = n_timestep - 1)
      if (!identical(x = implied_ALLTIMEt, y = correct_ALLTIMEt)) {
        stop(cat("The user-provided ALLTIMEt set steps for",
                 dQuote(x = "LRORG"),
                 "are not consistent with the 'n_timestep' value of:",
                 n_timestep,
                 "\nThere should be a",
                 dQuote(x = "Value"),
                 "entry for every ALLTIMEt set:",
                 toString(x = correct_ALLTIMEt)))
      }
    }
  }
}
if (grepl(pattern = "\\.csv", x = KAPPA)) {
  if (!file.exists(KAPPA)) {
    stop(paste("Filepath for",
               dQuote(x = "KAPPA"),
               "is not found."))
  } else {
    KAPPA_file <- read.csv(file = KAPPA)
    if (!all(is.element(el = colnames(x = KAPPA_file),
               set = c("REGr", "ALLTIMEt", "Value")))) {
      stop(paste("The user-provided intertemporal parameter file for",
                 dQuote(x = "KAPPA"),
                 "does not have one or more of the required columns: REGr, ALLTIMEt, Value"))
    } else {
      implied_ALLTIMEt <- as.integer(x = sort(x = unique(x = KAPPA_file[["ALLTIMEt"]])))
      correct_ALLTIMEt <- seq(from = 0, to = n_timestep - 1)
      if (!identical(x = implied_ALLTIMEt, y = correct_ALLTIMEt)) {
        stop(cat("The user-provided ALLTIMEt set steps for",
                 dQuote(x = "KAPPA"),
                 "are not consistent with the 'n_timestep' value of:",
                 n_timestep,
                 "\nThere should be a",
                 dQuote(x = "Value"),
                 "entry for every REGr by",
                 toString(x = correct_ALLTIMEt)))
      }
    }
  }
}
time_config <- list(time_steps = time_steps,
                    t0 = t0,
                    CPHI = CPHI,
                    KAPPA = KAPPA,
                    LRORG = LRORG,
                    INIDELTA = INIDELTA)
time_config
}
