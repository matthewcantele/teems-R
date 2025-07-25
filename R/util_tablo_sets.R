#' @importFrom purrr map
#'
#' @keywords internal
#' @noRd
.tablo_sets <- function(tab_extract,
                        call) {
  sets <- subset(tab_extract, tolower(type) %in% "set")

  # qualifier_arg
  # fill empty qualifier with implicit value (non_intertemporal)
  # handle mssing set label
  sets[["qualifier"]] <- ifelse(
    substr(sets$remainder, 1, 1) == "(",
    .get_element(
      input = sets[["remainder"]],
      split = " ",
      index = 1
    ),
    NA
  )

  sets[["qualifier"]] <- ifelse(is.na(sets$qualifier),
                                "(non_intertemporal)",
                                sets$qualifier)

  # check validity of qualifier (must be intertemporal or non_intertemporal)
  valid_qual <- c("(intertemporal)", "(non_intertemporal)")
  if (!any(is.element(el = tolower(x = sets[["qualifier"]]), set = valid_qual))) {
    invalid_qual <- sets[["qualifier"]][!is.element(el = tolower(x = sets[["qualifier"]]), set = valid_qual)]
    .cli_action(
      msg = "Invalid set qualifier detected: {.val {invalid_qual}}.",
      action = "abort",
      call = call
    )
  }

  # clear qualifier from remainder
  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["qualifier"]]
  )
  
  sets[["name"]] <- ifelse(grepl("#", sets$remainder),
    .get_element(
      input = sets[["remainder"]],
      split = "#",
      index = 1
    ),
    ifelse(grepl("\\(", sets$remainder),
      .get_element(
        input = sets[["remainder"]],
        split = "\\(",
        index = 1
      ),
      .get_element(
        input = sets[["remainder"]],
        split = "=",
        index = 1
      )
    )
  )

  sets[["name"]] <- trimws(sets$name)

  # clear name from remainder
  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["name"]]
  )

  # get descriptive info
  sets[["label"]] <- ifelse(grepl("#", sets$remainder),
    trimws(purrr::map(strsplit(sets$remainder, "#"), 2)),
    NA
  )
  
  # clear label from remainder
  sets[["remainder"]] <- .advance_remainder(
    remainder = sets[["remainder"]],
    pattern = sets[["label"]]
  )

  sets[["remainder"]] <- trimws(x = gsub(
    pattern = "#",
    replacement = "",
    x = sets[["remainder"]]
  ))
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
    .cli_action(
      msg = "Remnant set label detected during Tablo parsing.",
      action = "abort",
      call = call
    )
  } else {
    sets <- subset(x = sets, select = -remainder)
  }

  # check for incompatible Tablo statements
  if (any(sapply(
    sets[["definition"]],
    function(entry) {
      sum(grepl(
        pattern = "\\+",
        x = unlist(x = strsplit(x = entry, ""))
      )) >= 2
    }
  ))) {
    .cli_action(
      msg = c("Multiple {.val +} and/or {.val -} were detected within
                a single Tablo Set statement.", "For compatibility, split into
                multiple statements:\nInstead of A123=A1+A2+A3, A12=A1+A2 then
                A123=A12+A3"),
      action = c("abort", "inform"),
      call = call
    )
  }

  lapply(sets[["definition"]], function(entry) {
    if (!is.na(x = entry)) {
      if (!any(grepl(
        pattern = "\\+|\\-|union|intersect",
        x = entry,
        ignore.case = TRUE
      ))) {
        if (grepl(pattern = "=", x = entry)) {
          .cli_action(
            msg = c("It appears that one set has been defined as
                              identical to a second set (e.g., Set SET_B
                              # example set B # = SET_A;).", "If duplicate sets
                              are desired, multiple Read statements should be
                              implemented (e.g., Set SET_A # example set A #
                              maximum size 5 read elements from file GTAPSETS
                              header \"H2\";Set SET_B # example set B #
                              maximum size 5 read elements from file GTAPSETS
                              header \"H2\";)"),
            action = c("abort", "inform"),
            call = call
          )
        }
      }
    }
  })
  
  # remove "=,),(" from definitions
  sets[["definition"]] <- trimws(x = gsub(pattern = "\\(|=|\\)", replacement = "", x = sets[["definition"]]))
  sets[["definition"]] <- ifelse(test = grepl(pattern = ",", sets[["definition"]]),
                                 yes = strsplit(x = sets[["definition"]], split = ","),
                                 no = sets[["definition"]])
  sets[["definition"]] <- lapply(X = sets[["definition"]], FUN = trimws)
  names(sets[["definition"]]) <- sets[["name"]]
  sets[["intertemporal"]] <- is.element(el = sets[["qualifier"]], set = "(intertemporal)")
  sets[["data_type"]] <- "set"
  # other checks should include
  # le/ge/lt/gt in the RHS of formula
  # summation in formula headers

  # Subset statements ##########################################################
  subsets <- subset(
    x = tab_extract,
    subset = {
      is.element(
        el = tolower(x = type),
        set = "subset"
      )
    }
  )

  if (any(grepl(pattern = "\\(by numbers\\)", subsets[["remainder"]]))) {
    .cli_action(
      msg = "Subset '(by numbers)' argument not supported.",
      action = "abort",
      call = call
    )
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

  set_info <- list(
    sets = sets,
    subsets = subsets
  )

  return(set_info)
}