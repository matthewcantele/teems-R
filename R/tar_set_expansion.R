#' ExpandSets function
#'
#' This function expands sets based on their definitions. It handles both
#' non-inter-temporal and inter-temporal sets. It also sorts sets, handles
#' compound set components, and generates sets based on set formulas.
#'
#' @param nonint_sets A data frame of non-inter-temporal sets.
#' @param int_sets A data frame of inter-temporal sets, default is NULL.
#' @param set_extract A list containing set information to be expanded.
#'
#' @importFrom purrr pmap list_flatten
#' @return A list of expanded sets.
#' @keywords internal
#' @noRd
.expand_sets <- function(nonint_sets,
                       int_sets = NULL,
                       set_extract) {
  if (!is.null(x = int_sets)) {
    sets <- rbind(nonint_sets, int_sets)
  } else {
    sets <- nonint_sets
  }

  # remove "=,),(" from definitions
  set_extract[["definition"]] <- trimws(x = gsub(pattern = "\\(|=|\\)", replacement = "", x = set_extract[["definition"]]))

  set_extract[["definition"]] <- ifelse(test = grepl(pattern = ",", set_extract[["definition"]]),
                                        yes = strsplit(x = set_extract[["definition"]], split = ","),
                                        no = set_extract[["definition"]])

  # bring data from read-in sets over
  r_idx <- match(x = set_extract[["name"]], table = sets[["set_name"]])
  set_extract[["elements"]] <- lapply(X = sets[["dt"]], FUN = function(d) {
    d[[1]]
  })[r_idx]

  # recreate dependent sets from definition
  set_extract[["operator"]] <-
    ifelse(grepl(pattern = "UNION", set_extract[["definition"]], ignore.case = TRUE),
      "union",
      ifelse(
        grepl(pattern = "\\+", set_extract[["definition"]]),
        "c",
        ifelse(
          grepl(pattern = "\\-", set_extract[["definition"]]),
          "setdiff",
          ifelse(grepl(pattern = "\\INTERSECT", set_extract[["definition"]], ignore.case = TRUE),
            "intersect", NA
          )
        )
      )
    )

  # sort sets that will be sorted (note explicitly defined sets not sorted)
  set_extract[["elements"]] <- sapply(X = set_extract[["elements"]], FUN = sort)

  # sets that are explicitly defined within the tab file
  # non_intertemporal sets tolower
  set_extract[["elements"]] <- purrr::pmap(
    .l = list(
      set_extract[["elements"]],
      set_extract[["operator"]],
      set_extract[["definition"]],
      set_extract[["qualifier"]]
    ),
    .f = function(ele, op, def, qual) {
      if (is.null(x = ele) && is.na(x = op)) {
        trimws(x = tolower(x = def))
      } else {
        if (identical(x = qual, y = "(non_intertemporal)")) {
          tolower(x = ele)
        } else {
          return(ele)
        }
      }
    }
  )

  # compound set components
  set_extract[["comp"]] <- purrr::pmap(
    .l = list(
      set_extract[["operator"]],
      set_extract[["definition"]],
      set_extract[["qualifier"]]
    ),
    .f = function(op, def, qual) {
      if (!is.na(x = op) && qual != "(intertemporal)") {
        strsplit(x = def, split = "(?i)UNION|\\+|\\-|INTERSECT")
      } else {
        NA
      }
    }
  )

  set_extract[["comp"]] <- ifelse(!is.na(x = set_extract[["comp"]]),
    purrr::list_flatten(set_extract[["comp"]]),
    NA
  )

  # get first component of compound set definition
  set_extract[["comp1"]] <- ifelse(!is.na(x = set_extract[["comp"]]),
    trimws(x = unlist(x = lapply(X = set_extract[["comp"]], function(x) {
      x[1]
    }))),
    NA
  )

  # second component
  set_extract[["comp2"]] <- ifelse(!is.na(x = set_extract[["comp"]]),
    trimws(x = unlist(x = lapply(X = set_extract[["comp"]], FUN = function(x) {
      x[2]
    }))),
    NA
  )

  names(x = set_extract[["elements"]]) <- set_extract[["name"]]

  # complicated while loop here to generate sets based on set formulas iteratively
  # until all sets are reconstructed
  while (any(sapply(X = set_extract[["elements"]], FUN = identical, character(0)))) {
    set_extract[["elements"]] <- purrr::pmap(
      .l = list(
        set_extract[["operator"]],
        set_extract[["comp1"]],
        set_extract[["comp2"]],
        set_extract[["elements"]]
      ),
      .f = function(op, c1, c2, ele) {
        if (!is.na(x = op) && identical(x = ele, y = character(0))) {
          if (!identical(x = with(data = set_extract[["elements"]], expr = get(x = c1)), y = character(0)) &&
            !identical(x = with(data = set_extract[["elements"]], expr = get(x = c2)), y = character(0))) {
            with(data = set_extract[["elements"]], expr = eval(expr = parse(text = paste0(op, "(", c1, ",", c2, ")"))))
          } else {
            ele
          }
        } else {
          ele
        }
      }
    )
    names(x = set_extract[["elements"]]) <- set_extract[["name"]]
  }

  # post model sets all lowercase
  set_extract[["name"]] <- tolower(x = set_extract[["name"]])

  # check whether any sets were not able to be constructed
  if (any(sapply(X = set_extract[["elements"]], FUN = function(e) {
    any(is.na(x = e))
  }))) {
    stop("Set construction of dependent (not read-in) sets was not possible")
  }

  # bind non-int set mapping to a single object
  r_idx <- match(x = toupper(x = set_extract[["header"]]), table = toupper(x = nonint_sets[["header"]]))
  set_extract[["full_sets"]] <- nonint_sets[["full_sets"]][r_idx]

  return(set_extract)
}
