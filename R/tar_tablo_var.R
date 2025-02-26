#' .parse_tablo function
#'
#' This function extracts variable information from the model Tablo file.
#'
#' @param parsed_tablo Parsed Tablo file produced by \code{.parse_tablo()}.
#'
#' @importFrom purrr map
#' @return A list containing the original Tablo data, the extracted components,
#'   and the file name.
#' @keywords internal
#' @noRd
.tablo_variables <- function(parsed_tablo) {

  var <- subset(x = parsed_tablo, subset = {
    is.element(el = tolower(x = type), set = "variable")
  })

  # check that information is available for each variable and coefficient
  if (any(!grepl("#", var[["remainder"]]))) {
    stop("One or more variables or coefficients is missing a information - add # NA # if not descriptive information is available.")
  }

  var[["information"]] <- paste(
    "#",
    trimws(x = unlist(x = purrr::map(.x = sapply(
      X = var[["remainder"]],
      FUN = strsplit, split = "#"
    ), 2))),
    "#"
  )

  var[["remainder"]] <- trimws(x = unlist(x = purrr::map(.x = sapply(
    X = var[["remainder"]],
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
  first_enclosure <- sapply(X = strsplit(x = var[["remainder"]], ")"), "[[", 1)

  var[["qualifier_list"]] <- ifelse(test = grepl(
    pattern = paste(qual, collapse = "|"),
    x = first_enclosure,
    ignore.case = TRUE
  ),
  yes = paste0(first_enclosure, ")"),
  no = NA
  )

  var[["remainder"]] <- .advance_remainder(remainder = var[["remainder"]],
                                           pattern = var[["qualifier_list"]])

  # pull out names
  var[["name"]] <- unlist(x = purrr::map(
    .x = sapply(X = var[["remainder"]], FUN = strsplit, split = " "),
    .f = function(x) {
      tail(x, 1)
    }
  ))

  # subindex (c,a,r)
  var[["lower_idx"]] <- unlist(x = ifelse(test = grepl(pattern = "\\(", x = var[["name"]]),
    yes = paste0("(", lapply(
      X = strsplit(x = var[["name"]], split = "\\("),
      FUN = function(s) {
        s[2]
      }
    )),
    no = "null_set"
  ))

  # subindex listed
  var[["ls_lower_idx"]] <- sapply(X = var[["lower_idx"]], FUN = function(si) {
    si <- gsub(pattern = "\\(|\\)", replacement = "", x = si)
    strsplit(x = si, split = ",")
  }, USE.NAMES = FALSE)

  names(x = var[["ls_lower_idx"]]) <- var[["name"]]

  var[["remainder"]] <- .advance_remainder(remainder = var[["remainder"]],
                                           pattern = var[["name"]])

  # remove subindex from name
  var[["name"]] <- unlist(x = purrr::map(.x = sapply(X = var[["name"]], FUN = strsplit, split = "\\("), 1))


  # main index (COMM,ACTS,REG)
  var[["ls_upper_idx"]] <- ifelse(test = var[["remainder"]] == "",
    yes = "null_set",
    no = sapply(X = sapply(X = var[["remainder"]], FUN = strsplit, split = ")"), FUN = function(ss) {
      sapply(strsplit(x = ss, split = ","), "[[", 3)
    })
  )

  names(x = var[["ls_upper_idx"]]) <- var[["name"]]

  # ls_upper_idx in concat form
  var[["upper_idx"]] <- sapply(
    X = var[["ls_upper_idx"]],
    FUN = function(s) {
      if (any(!is.element(el = "null_set", s))) {
        paste0("(", paste0(s, collapse = ","), ")")
      } else {
        "null_set"
      }
    }
  )

  # mixed concat
  var[["mixed_idx"]] <- unlist(x = purrr::map2(
    .x = var[["ls_upper_idx"]],
    .y = var[["ls_lower_idx"]],
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
  var[["ls_mixed_idx"]] <- strsplit(x = var[["mixed_idx"]], split = ",")

  var[["mixed_idx"]] <- paste0("(", var[["mixed_idx"]], ")")

  # full standard writeout
  data.table::setnames(x = var,
                       old = "remainder",
                       new = "full_set")

  # Read statements ############################################################
  r <- subset(parsed_tablo, subset = {
    tolower(x = type) == "read"
  })
  r[["name"]] <- trimws(x = purrr::map(.x = sapply(
    X = toupper(x = r[["remainder"]]),
    FUN = strsplit,
    split = "FROM FILE"
  ), 1))

  r[["remainder"]] <- .advance_remainder(remainder = r[["remainder"]],
                                         pattern = paste(r[["name"]], "from file"))

  r[["file"]] <- .get_element(input = r[["remainder"]], split = " ", index = 1)
  r[["header"]] <- gsub(
    pattern = "\"",
    replacement = "",
    x = .get_element(input = r[["remainder"]], split = " ", index = 3)
  )

  # field to mark coefficients read in to a different name (VTWR to VTMFSD)
  r[["diff"]] <- r[["name"]] == r[["header"]]
  r <- subset(x = r, select = -remainder)

  # match read statements to associated coefficients
  r_idx <- match(x = var[["name"]], table = r[["name"]])
  var[["header"]] <- r[["header"]][r_idx]
  var[["file"]] <- r[["file"]][r_idx]

  data.table::setcolorder(x = var, neworder = c("name",
                                                "header",
                                                "information",
                                                "file",
                                                "qualifier_list",
                                                "full_set",
                                                "mixed_idx",
                                                "upper_idx",
                                                "lower_idx",
                                                "ls_mixed_idx",
                                                "ls_upper_idx",
                                                "ls_lower_idx"))


  return(var)
}
