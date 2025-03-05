#' @importFrom purrr pluck transpose simplify pmap
#' @importFrom tibble tibble
#' @importFrom data.table setkey
#' 
#' @keywords internal
#' @noRd
.parse_set <- function(paths,
                       sets,
                       call) {

  # check paths
  if (!all(sapply(X = paths, FUN = file.exists))) {
    .cli_action(
      action = "abort",
      msg = "One or more set file paths does not exist.",
      call = call
    )
  }
  
  # metadata in the first line of each "csv", rest is ragged data
  ls_data <- lapply(X = paths, FUN = readLines)

  list_set <- lapply(X = ls_data,
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

    list(name, information, dat)
  })

  transposed <- purrr::transpose(.l = list_set)

  # Create the tibble
  set_tib <- tibble::tibble(
    name = purrr::simplify(.x = transposed[[1]], .type = "character"),
    information = purrr::simplify(.x = transposed[[2]], .type = "character"),
    ls_data = transposed[[3]]
  )

  # check that sets tab extract sets (see previous check in .check_sets)
  if (!all(is.element(el = sets[["name"]], set = set_tib[["name"]]))) {
    .cli_action(
      action = "abort",
      msg = "One or more sets identified from the Tablo
                extract was not found in the output csvs.",
      call = call
    )
  }
  
  # check that the set ele order is as expected
  r_idx <- match(x = set_tib[["name"]], table = sets[["name"]])
  sets <- sets[r_idx,]

  set_tib[["dat"]] <- lapply(
    X = set_tib[["ls_data"]],
    FUN = function(ele) {
      dt <- data.table::data.table(Value = ele)
      if (all(!is.na(x = suppressWarnings(expr = as.numeric(x = ele))))) {
        dt[["Value"]] <- as.numeric(x = dt[["Value"]])
      }
      return(dt)
    }
  )
  
  purrr::pmap(
    .l = list(
      set_tib[["dat"]],
      sets[["elements"]],
      set_tib[["name"]]
    ),
    .f = function(ele_out, ele_in, set_name) {
      browser()
      ele_out <- unlist(x = ele_out, use.names = FALSE)
      if (!all.equal(ele_out, ele_in)) {
        .cli_action(
          action = "abort",
          msg = "Tablo-parsed sets and/or elements on the {.val {set_name}} set
          are not identical to post-model Tablo set writeouts. This is likely 
          an internal error and should be forwarded to the package maintainer."
        )
      }
    }
  )

  set_tib <- subset(x = set_tib, select = c(name, information, dat))
  return(set_tib)
}
