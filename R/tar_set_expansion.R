#' @importFrom purrr pmap list_flatten
#' 
#' @keywords internal
#' @noRd
.expand_sets <- function(sets,
                         set_extract) {

  r_idx <- ifelse(test = is.element(el = set_extract[["header"]],
                                    set = sets[["header"]]),
                  yes = match(table = sets[["header"]], x = set_extract[["header"]]),
                  no = ifelse(test = is.element(el = set_extract[["name"]],
                                                set = sets[["header"]]),
                              yes = match(table = sets[["header"]], x = set_extract[["name"]]),
                              no = NA))

  set_extract[["full_ele"]] <- lapply(X = sets[["dt"]], FUN = function(d) {
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
  set_extract[["full_ele"]] <- sapply(X = set_extract[["full_ele"]], FUN = sort)

  # sets that are explicitly defined within the tab file
  # non_intertemporal sets tolower
  set_extract[["full_ele"]] <- purrr::pmap(
    .l = list(
      set_extract[["full_ele"]],
      set_extract[["operator"]],
      set_extract[["definition"]],
      set_extract[["qualifier"]]
    ),
    .f = function(ele, op, def, qual) {
      if (is.null(x = ele) && is.na(x = op)) {
        trimws(x = tolower(x = def))
      } else {
        return(ele)
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

  names(x = set_extract[["full_ele"]]) <- set_extract[["name"]]

  # complicated while loop here to generate sets based on set formulas iteratively
  # counter here for exit after x num loops
  # until all sets are reconstructed
  counter <- 0
  while (any(sapply(X = set_extract[["full_ele"]], FUN = is.null))) {
    counter <- counter + 1
    if (counter > 20) {
      stop("Get dev to fix while loop in set expansion.")
    }
    set_extract[["full_ele"]] <- purrr::pmap(
      .l = list(
        set_extract[["operator"]],
        set_extract[["comp1"]],
        set_extract[["comp2"]],
        set_extract[["full_ele"]]
      ),
      .f = function(op, c1, c2, ele) {
        if (!is.na(x = op) && is.null(x = ele)) {
          x <- with(data = set_extract[["full_ele"]], expr = get(x = c1))
          y <- with(data = set_extract[["full_ele"]], expr = get(x = c2))
          if (!any(is.null(x = x), is.null(x = y))) {
            ele <- do.call(what = op, args = list(x, y))
          } else {
            return(NULL)
          }
        } else {
          return(ele)
        }
      }
    )
    names(x = set_extract[["full_ele"]]) <- set_extract[["name"]]
  }
  # check whether any sets were not able to be constructed
  if (any(sapply(X = set_extract[["full_ele"]], FUN = function(e) {
    any(is.na(x = e))
  }))) {
    stop("Set construction of dependent (not read-in) sets was not possible")
  }
  
  return(set_extract)
}
