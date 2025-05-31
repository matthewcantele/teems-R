#' @importFrom purrr map
#' 
#' @keywords internal
#' @noRd
.tablo_coeff <- function(tab_extract,
                         call) {

  coeff <- subset(
    x = tab_extract,
    subset = {
      is.element(
        el = tolower(x = type),
        set = "coefficient"
      )
    },
    select = -type
  )

  # check that label is available for each coefficient
  if (any(!grepl("#", coeff[["remainder"]]))) {
    .cli_action(msg = "One or more variables or coefficients within the provided
                Tablo file is missing label - add # NA # if descriptive 
                label is not available.",
                action = "abort",
                call = call)
  }

  coeff[["label"]] <- trimws(x = unlist(x = purrr::map(.x = sapply(
      X = coeff[["remainder"]],
      FUN = strsplit, split = "#",
      USE.NAMES = FALSE
    ), 2)))


  coeff[["remainder"]] <- trimws(x = unlist(x = purrr::map(.x = sapply(
    X = coeff[["remainder"]],
    FUN = strsplit, split = "#"
  ), 1)))

  # qualifiers
  # additional regex necessary for variables with parts that match below
  qual <- c(
    "nonzero_by_zero",
    "zero_by_zero",
    "\\bchange\\b",
    "linear",
    "orig_level",
    "ge [[:digit:]]",
    "gt [[:digit:]]",
    "integer",
    "parameter",
    "initial",
    "\\breal\\b",
    "levels"
  )

  # first parenthetic enclosure which may include a qualifier
  first_enclosure <- sapply(X = strsplit(x = coeff[["remainder"]], ")"), "[[", 1)

  coeff[["qualifier_list"]] <- ifelse(test = grepl(
    pattern = paste(qual, collapse = "|"),
    x = first_enclosure,
    ignore.case = TRUE
  ),
  yes = paste0(first_enclosure, ")"),
  no = NA
  )

  coeff[["remainder"]] <- .advance_remainder(remainder = coeff[["remainder"]],
                                             pattern = coeff[["qualifier_list"]])

  # pull out coefficients
  coeff[["coefficient"]] <- unlist(x = purrr::map(
    .x = sapply(X = coeff[["remainder"]], FUN = strsplit, split = " "),
    .f = function(x) {
      tail(x, 1)
    }
  ))

  # subindex (c,a,r)
  coeff[["lower_idx"]] <- unlist(x = ifelse(test = grepl(pattern = "\\(", x = coeff[["coefficient"]]),
    yes = paste0("(", lapply(
      X = strsplit(x = coeff[["coefficient"]], split = "\\("),
      FUN = function(s) {
        s[2]
      }
    )),
    no = "null_set"
  ))

  # subindex listed
  coeff[["ls_lower_idx"]] <- sapply(X = coeff[["lower_idx"]], FUN = function(si) {
    si <- gsub(pattern = "\\(|\\)", replacement = "", x = si)
    strsplit(x = si, split = ",")
  }, USE.NAMES = FALSE)

  coeff[["remainder"]] <- .advance_remainder(remainder = coeff[["remainder"]],
                                             pattern = coeff[["coefficient"]])

  # remove subindex from coefficient
  coeff[["coefficient"]] <- unlist(x = purrr::map(.x = sapply(X = coeff[["coefficient"]],
                                                              FUN = strsplit,
                                                              split = "\\(",
                                                              USE.NAMES = FALSE),
                                                  1))

  names(x = coeff[["ls_lower_idx"]]) <- coeff[["coefficient"]]
  
  # main index (COMM,ACTS,REG)
  coeff[["ls_upper_idx"]] <- ifelse(test = coeff[["remainder"]] == "",
    yes = "null_set",
    no = sapply(X = sapply(X = coeff[["remainder"]], FUN = strsplit, split = ")"), FUN = function(ss) {
      sapply(strsplit(x = ss, split = ","), "[[", 3)
    })
  )

  names(x = coeff[["ls_upper_idx"]]) <- coeff[["coefficient"]]

  # ls_upper_idx in concat form
  coeff[["upper_idx"]] <- sapply(
    X = coeff[["ls_upper_idx"]],
    FUN = function(s) {
      if (any(!is.element(el = "null_set", s))) {
        paste0("(", paste0(s, collapse = ","), ")")
      } else {
        "null_set"
      }
    }
  )

  # mixed concat
  coeff[["mixed_idx"]] <- unlist(x = purrr::map2(
    .x = coeff[["ls_upper_idx"]],
    .y = coeff[["ls_lower_idx"]],
    .f = function(sup, sub) {
      if (unlist(x = !any(sup == "null_set"))) {
        paste(map2(sup, sub, function(sup2, sub2) {
          paste0(sup2, sub2)
        }), collapse = ",")
      } else {
        "null_set"
      }
    }
  ))

  # list shock indices
  coeff[["ls_mixed_idx"]] <- strsplit(x = coeff[["mixed_idx"]], split = ",")

  coeff[["mixed_idx"]] <- paste0("(", coeff[["mixed_idx"]], ")")
  coeff[["data_type"]] <- ifelse(test = grepl(pattern = "parameter", coeff[["qualifier_list"]]),
                                 yes = "par",
                                 no = "dat")
  # full standard writeout
  data.table::setnames(x = coeff,
                       old = "remainder",
                       new = "full_set")

  # Read statements ############################################################
  r <- subset(x = tab_extract, subset = {
    tolower(x = type) == "read"
  })

  r[["coefficient"]] <- trimws(x = purrr::map(.x = sapply(
    X = toupper(x = r[["remainder"]]),
    FUN = strsplit,
    split = "FROM FILE"
  ), 1))

  r[["remainder"]] <- .advance_remainder(remainder = r[["remainder"]],
                                         pattern = paste(r[["coefficient"]], "from file"))

  r[["file"]] <- .get_element(input = r[["remainder"]], split = " ", index = 1)
  r[["header"]] <- gsub(
    pattern = "\"",
    replacement = "",
    x = .get_element(input = r[["remainder"]], split = " ", index = 3)
  )

  # field to mark coefficients read in to a different name (VTWR to VTMFSD)
  r[["diff"]] <- r[["coefficient"]] == r[["header"]]
  r <- subset(x = r, select = -remainder)

  # match read statements to associated coefficients
  r_idx <- match(x = coeff[["coefficient"]], table = r[["coefficient"]])
  coeff[["header"]] <- r[["header"]][r_idx]
  coeff[["file"]] <- r[["file"]][r_idx]

  data.table::setcolorder(x = coeff, neworder = c("coefficient",
                                                  "header",
                                                  "label",
                                                  "file",
                                                  "data_type",
                                                  "qualifier_list",
                                                  "full_set",
                                                  "mixed_idx",
                                                  "upper_idx",
                                                  "lower_idx",
                                                  "ls_mixed_idx",
                                                  "ls_upper_idx",
                                                  "ls_lower_idx"))
  
  return(coeff)
}
