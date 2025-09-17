#' @importFrom purrr pluck list_flatten map map_chr map2
#'
#' @keywords internal
#' @noRd
.parse_tab_obj <- function(extract,
                           obj_type,
                           call) {
  obj <- subset(extract, tolower(type) == obj_type)
  obj$remainder <- purrr::map_chr(
    obj$remainder,
    function(r) {
      if (!grepl("#", r)) {
        r <- paste(r, "# NA #")
      }
      return(r)
    }
  )

  obj$label <- purrr::map_chr(
    obj$remainder,
    function(r) {
      trimws(purrr::map_chr(strsplit(r, "#"), 2))
    }
  )

  obj$remainder <- purrr::map_chr(
    obj$remainder,
    function(r) {
      trimws(purrr::map_chr(strsplit(r, "#"), 1))
    }
  )

  first_enclosure <- purrr::map_chr(
    obj$remainder,
    function(r) {
      purrr::map_chr(strsplit(r, ")"), 1)
    }
  )

  obj$qualifier_list <- ifelse(grepl(
    paste(tab_qual, collapse = "|"),
    first_enclosure,
    ignore.case = TRUE
  ),
  paste0(first_enclosure, ")"),
  NA
  )

  obj$remainder <- .advance_remainder(
    remainder = obj$remainder,
    pattern = obj$qualifier_list
  )

  obj$name <- purrr::map_chr(
    obj$remainder,
    function(r) {
      if (grepl(" ", r)) {
        purrr::map_chr(strsplit(r, " "), 2)
      } else {
        r
      }
    }
  )

  obj$lower_idx <- purrr::map_chr(obj$name, function(n) {
    if (grepl("\\(", n)) {
      paste0("(", purrr::pluck(strsplit(n, "\\("), 1, 2))
    } else {
      NA
    }
  })

  obj$ls_lower_idx <- purrr::list_flatten(purrr::map(
    obj$lower_idx,
    function(i) {
      strsplit(gsub("\\(|\\)", "", i), ",")
    }
  ))

  obj$remainder <- .advance_remainder(
    remainder = obj$remainder,
    pattern = obj$name
  )

  obj$name <- purrr::map_chr(
    obj$name,
    function(n) {
      purrr::map_chr(strsplit(n, "\\("), 1)
    }
  )

  obj$ls_upper_idx <- ifelse(obj$remainder != "",
    sapply(sapply(obj$remainder, strsplit, split = ")"), function(ss) {
      sapply(strsplit(ss, ","), "[[", 3)
    }),
    NA
  )

  obj$upper_idx <- purrr::map(
    obj$ls_upper_idx,
    function(s) {
      if (!s %=% NA) {
        paste0("(", paste0(s, collapse = ","), ")")
      } else {
        NA
      }
    }
  )

  obj$mixed_idx <- unlist(x = purrr::map2(
    obj$ls_upper_idx,
    obj$ls_lower_idx,
    .f = function(up, low) {
      if (!up %=% NA && !low %=% NA) {
        paste(map2(up, low, function(up2, low2) {
          paste0(up2, low2)
        }), collapse = ",")
      } else {
        NA
      }
    }
  ))

  obj$ls_mixed_idx <- strsplit(obj$mixed_idx, ",")
  obj$mixed_idx <- paste0("(", obj$mixed_idx, ")")

  if (obj_type %=% "coefficient") {
    r <- subset(extract, tolower(type) == "read")

    r$name <- trimws(purrr::map_chr(purrr::map(purrr::map(
      toupper(r$remainder),
      strsplit,
      "FROM FILE"
    ), 1), 1))

    r$remainder <- .advance_remainder(
      remainder = r$remainder,
      pattern = paste(r$name, "from file")
    )

    r$file <- .get_element(input = r$remainder, split = " ", index = 1)
    r$header <- gsub(
      pattern = "\"",
      replacement = "",
      x = .get_element(input = r$remainder, split = " ", index = 3)
    )

    r_idx <- match(obj$name, r$name)
    obj$header <- r$header[r_idx]
    obj$file <- r$file[r_idx]
  } else if (obj_type %=% "variable") {
    obj$name <- tolower(obj$name)
    obj$header <- NA
    obj$file <- NA
  }
  
  names(obj$ls_upper_idx) <- obj$name
  names(obj$ls_mixed_idx) <- obj$name
  
  obj$definition <- NA
  obj$comp1 <- NA
  obj$comp2 <- NA
  obj$subsets <- NA
  
  # some permutations not yet utilized
  obj <- subset(obj,
    select = c(type, name, label, qualifier_list, ls_upper_idx, ls_mixed_idx, header, file, definition, subsets, comp1, comp2, row_id)
  )

  return(obj)
}
