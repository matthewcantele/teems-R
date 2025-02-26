#' .parse_tablo function
#'
#' This function extracts key set and subset components from the model Tablo
#' file.
#'
#' @param parsed_tablo Parsed Tablo file produced by \code{.parse_tablo()}.
#'
#' @importFrom purrr map
#' @return A list containing the set and subset information for a given Tablo
#'   file.
#' @keywords internal
#' @noRd
.tablo_sets <- function(parsed_tablo) {
  # Set statements
  sets <- subset(
    x = parsed_tablo,
    subset = {
      is.element(
        el = tolower(x = type),
        set = "set"
      )
    }
  )

  # qualifier_arg
  # fill empty qualifier with implicit value (non_intertemporal)
  sets[["qualifier"]] <- ifelse(
    test = grepl(pattern = "\\(|\\)", x = .get_element(sets[["remainder"]], " ", 1)),
    yes = .get_element(
      input = sets[["remainder"]],
      split = " ",
      index = 1
    ),
    no = "(non_intertemporal)"
  )

  # check validity of qualifier (must be intertemporal or non_intertemporal)
  if (!any(is.element(
    el = tolower(x = sets[["qualifier"]]),
    set = c("(intertemporal)", "(non_intertemporal)")
  ))) {
    stop("Invalid Set qualifier detected.")
  }

  # clear qualifier from remainder
  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["qualifier"]]
  )

  # Set names
  sets[["name"]] <- .get_element(
    input = sets[["remainder"]],
    split = " ",
    index = 1
  )

  # clear name from remainder
  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["name"]]
  )

  # get descriptive info
  sets[["information"]] <- paste0(
    "#",
    .get_element(input = sets[["remainder"]], split = "#", index = 2, fixed = TRUE),
    "#"
  )

  # clear information from remainder
  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["information"]]
  )

  # maximum size if present (note that this feature does not appear to have any interaction with our solver)
  sets[["max_size"]] <- unlist(x = ifelse(
    test = grepl(
      pattern = "maximum size",
      x = sets[["remainder"]],
      ignore.case = TRUE
    ),
    yes = paste("maximum size", purrr::map(.x = strsplit(
      x = sets[["remainder"]], split = " "
    ), 3)),
    no = NA
  ))

  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["max_size"]]
  )

  # read elements
  sets[["full_read"]] <- ifelse(
    test = grepl(pattern = "read elements from file", x = tolower(x = sets[["remainder"]])),
    yes = sets[["remainder"]],
    no = NA
  )

  sets[["file"]] <- ifelse(
    test = !is.na(x = sets[["full_read"]]),
    yes = unlist(x = purrr::map(
      .x = strsplit(x = toupper(x = sets[["full_read"]]), split = toupper(x = "from file")),
      .f = function(s) {
        trimws(x = purrr::map(strsplit(
          x = s[2], split = toupper("header")
        ), 1))
      }
    )),
    no = NA
  )

  sets[["header"]] <- ifelse(test = !is.na(x = sets[["full_read"]]),
    yes = unlist(x = purrr::map(
      .x = strsplit(x = toupper(x = sets[["full_read"]]), split = toupper(x = "from file")),
      .f = function(s) {
        trimws(x = purrr::map(strsplit(x = s[2], split = toupper("header")), 2))
      }
    )),
    no = NA
  )

  sets[["header"]] <- gsub(pattern = "\"", replacement = "", x = sets[["header"]])

  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["full_read"]]
  )

  sets[["size"]] <- ifelse(test = grepl(pattern = "size", x = tolower(x = sets[["remainder"]])),
    yes = trimws(x = purrr::map(.x = strsplit(x = sets[["remainder"]], split = "\\("), 1)),
    no = NA
  )

  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["size"]]
  )

  sets[["definition"]] <- ifelse(test = sets[["remainder"]] != "",
    yes = sets[["remainder"]],
    no = NA
  )

  # final set check
  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["definition"]]
  )

  if (any(!is.element(el = "", set = sets[["remainder"]]))) {
    stop("Remnant Set information detected from Tablo parse.")
  } else {
    sets <- subset(x = sets, select = -remainder)
  }

  # check for incompatible Tablo statements
  if (any(sapply(sets[["definition"]],
                 function(entry) {
                   sum(grepl(pattern = "\\+",
                             x = unlist(x = strsplit(x = entry, "")))) >= 2
                 }))) {
    stop(paste("Multiple",
               dQuote(x = "+"),
               "and/or",
               dQuote(x = "-"),
               "were detected within a single Tablo Set statement.",
               "For compatibility, split into multiple statements:",
               "Instead of A123=A1+A2+A3, A12=A1+A2 then A123=A12+A3"))
  }

  lapply(sets[["definition"]], function(entry) {
    if (!is.na(x = entry)) {
      if (!any(grepl(
        pattern = "\\+|\\-|union|intersect",
        x = entry,
        ignore.case = TRUE
      ))) {
        if (grepl(pattern = "=", x = entry)) {
          stop(
            "It appears that one set has been defined as identical to a second set. If duplicate sets are desired, multiple Read statements should be implemented."
          )
        }
      }
    }
  })

  sets[["intertemporal"]] <- is.element(el = sets[["qualifier"]], set = "(intertemporal)")
  sets[["data_type"]] <- "set"
  # other checks should include
  # le/ge/lt/gt in the RHS of formula
  # summation in formula headers

  # Subset statements ##########################################################
  subsets <- subset(
    x = parsed_tablo,
    subset = {
      is.element(
        el = tolower(x = type),
        set = "subset"
      )
    }
  )

  if (any(grepl(pattern = "\\(by numbers\\)", subsets[["remainder"]]))) {
    stop("Subset '(by numbers)' argument not supported.")
  }

  subsets[["subset"]] <- trimws(x = purrr::map(sapply(
    X = toupper(subsets[["remainder"]]),
    FUN = strsplit,
    split = toupper("is subset of")
  ), 1))

  subsets[["set"]] <- trimws(x = purrr::map(sapply(
    X = toupper(subsets[["remainder"]]),
    FUN = strsplit,
    split = toupper("is subset of")
  ), 2))

  subsets <- subset(x = subsets, select = -remainder)

  set_info <- list(sets = sets,
                   subsets = subsets)

  return(set_info)
}
