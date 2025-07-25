#' @importFrom purrr map2 pmap
#' 
#' @keywords internal
#' @noRd
.tablo_maths <- function(tab_extract,
                         call) {

  maths <- subset(tab_extract, tolower(type) %in% c("equation", "formula"))

  maths[["name"]] <- unlist(x = purrr::map2(
    .x = maths[["type"]],
    .y = maths[["remainder"]],
    .f = function(t, r) {
      if (identical(x = t, y = "Equation")) {
        lapply(X = strsplit(x = r, split = " "), "[[", 1)
      } else {
        NA
      }
    }
  ))
  # info check
  # equations <- maths$remainder[maths$type == "Equation"]
  # if (!all(lengths(gregexpr("#", equations, fixed = TRUE)) == 2)) {
  #   # missing label
  #   m_eq <- equations[lengths(x = gregexpr("#", equations, fixed = TRUE)) != 2]
  #   .cli_action(msg = "The following equations are missing a label: 
  #               {.val {m_eq}}. For no label use e.g., # NA #.",
  #               action = "inform")
  # }
  
  maths[["remainder"]] <- unlist(x = purrr::map2(
    .x = maths[["remainder"]],
    .y = maths[["name"]],
    .f = function(r, n) {
      if (!is.na(x = n)) {
        return(trimws(sub(pattern = n, "", r, fixed = TRUE)))
      } else {
        return(r)
      }
    }
  ))

  maths[["label"]] <- sapply(
    X = strsplit(
      x = maths[["remainder"]],
      split = "#"
    ),
    FUN = function(spl) {
      if (length(x = spl) > 1) {
        trimws(x = spl[2])
      } else {
        NA
      }
    }
  )

  maths[["remainder"]] <- unlist(x = pmap(
    .l = list(
      maths[["remainder"]],
      maths[["label"]],
      maths[["type"]]
    ),
    .f = function(rem, info, t) {
      if (identical(x = t, y = "Equation")) {
        if (grepl(pattern = "#", x = rem)) {
        rem <- strsplit(x = rem, split = "#")[[1]][3]
        trimws(x = rem)
        } else {
          return(rem)
        }
      } else {
        return(rem)
      }
    }
  ))
  
  # from util_tablo_var (make function)
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
  first_enclosure <- lapply(X = strsplit(x = maths[["remainder"]], ")"), "[[", 1)
  
  maths[["qualifier_list"]] <- ifelse(test = grepl(
    pattern = paste(qual, collapse = "|"),
    x = first_enclosure,
    ignore.case = TRUE
  ),
  yes = paste0(first_enclosure, ")"),
  no = NA
  )
  
  maths[["remainder"]] <- .advance_remainder(
    remainder = maths[["remainder"]],
    pattern = maths[["qualifier_list"]]
  )
  
  maths[["full_set"]] <- sapply(X = maths[["remainder"]],
                                FUN = .cleave_math,
                                pos = 2L,
                                USE.NAMES = FALSE)
  
  maths[["math"]] <- sapply(X = maths[["remainder"]],
                            FUN = .cleave_math,
                            pos = 3L,
                            USE.NAMES = FALSE)

  maths[["math"]] <- ifelse(test = is.na(x = maths[["full_set"]]),
                            yes = maths[["remainder"]],
                            no = maths[["math"]])
  
  maths[["LHS"]] <- trimws(x = sapply(X = strsplit(x = maths[["math"]],
                                                   split = "="),
                                      FUN = "[[", 1))
  
  maths[["RHS"]] <- trimws(x = sapply(X = strsplit(x = maths[["math"]],
                                                   split = "="),
                                      FUN = "[[", 2))
  

  maths[["full_set"]] <- ifelse(test = is.na(x = maths[["full_set"]]),
                                yes = "null_set",
                                no = maths[["full_set"]])
  
  return(maths)
}
