#' @importFrom data.table fsetdiff copy fsetequal
#' @importFrom purrr list_flatten map2_lgl compact map_chr
#' 
#' @keywords internal
#' @noRd
.reduce2sets <- function(preswap,
                         swap,
                         reduced_entry) {
  UseMethod(".reduce2sets")
}

#' @method .reduce2sets default
#' @export
.reduce2sets.default <- function(preswap,
                                 swap,
                                 reduced_entry) {
  # expand methods and keep subsets where possible
  swapped_sets <- attr(preswap, "comp")[attr(swap, "comp") != attr(preswap, "comp")]

  if (inherits(preswap, "subset")) {
    swapped_sets <- colnames(attr(preswap, "ele"))[match(swapped_sets, attr(preswap, "comp"))]
  }
  var_name <- attr(preswap, "var_name")
  full_dt <- attr(preswap, "ele")
  diff_dt <- data.table::fsetdiff(full_dt, attr(swap, "ele"))

  if (inherits(preswap, "mixed")) {
    mixed_comp <- which(attr(preswap, "comp") != colnames(attr(preswap, "ele")))
    colnames(full_dt)[mixed_comp] <- paste0("\"", attr(preswap, "comp")[mixed_comp], "\"")
    colnames(diff_dt)[mixed_comp] <- paste0("\"", attr(preswap, "comp")[mixed_comp], "\"")
  }

  for (d in seq_along(swapped_sets)) {
    set_name <- swapped_sets[d]

    if (d %=% 1L) {
      full_dt <- split(full_dt, by = set_name)
      diff_dt <- split(diff_dt, by = set_name)
      diff_dt <- diff_dt[match(names(full_dt), names(diff_dt))]

      if (!all(names(full_dt) %in% names(diff_dt))) {
        null_sets <- data.table::copy(full_dt[!names(full_dt) %in% names(diff_dt)])
        lapply(null_sets, function(s) {
          cols <- names(s)
          na_values <- lapply(s, function(na) {
            switch(class(na)[1],
                   "character" = NA_character_,
                   "integer" = NA_integer_,
                   "numeric" = NA_real_,
                   "logical" = NA,
                   NA
            )
          })
          
          s[, (cols) := na_values]
        })
        diff_dt <- c(diff_dt, null_sets)
        diff_dt <- diff_dt[match(names(full_dt), names(diff_dt))]
      }
    } else {
      diff_dt <- purrr::list_flatten(lapply(diff_dt, split, by = set_name))
      full_dt <- purrr::list_flatten(lapply(full_dt, split, by = set_name))
      full_dt <- full_dt[names(full_dt) %in% names(diff_dt)]
    }

    complete <- purrr::map2_lgl(
      full_dt,
      diff_dt,
      data.table::fsetequal
    )

    diff_dt <- lapply(diff_dt, function(subset) {
      if (!all(is.na(subset))) {
        replacement <- paste0("\"", unlist(unique(subset[, ..set_name])), "\"")
        colnames(x = subset) <- gsub(
          pattern = set_name,
          replacement = replacement,
          colnames(subset)
        )
      }
      return(subset)
    })

    full_dt <- lapply(full_dt, function(subset) {
      if (!all(is.na(subset))) {
        replacement <- paste0("\"", unlist(unique(subset[, ..set_name])), "\"")
        colnames(subset) <- gsub(
          pattern = set_name,
          replacement = replacement,
          colnames(subset)
        )
      }
      return(subset)
    })

    if (any(complete)) {
      ls_reduced <- diff_dt[complete]
      for (out in seq_along(ls_reduced)) {
        reduced_entry[length(reduced_entry) + 1] <- list(colnames(ls_reduced[[out]]))
      }
    }

    diff_dt <- purrr::compact(diff_dt[!complete])
    full_dt <- purrr::compact(full_dt[!complete])
  }

  reduced_entry <- purrr::map_chr(reduced_entry, function(c) {
    paste0(var_name, "(", paste(c, collapse = ","), ")")
  })

  return(reduced_entry)
}


#' @method .reduce2sets ele
#' @export
.reduce2sets.ele <- function(preswap,
                             swap,
                             reduced_entry) {
  if (!data.table::fsetequal(attr(preswap, "ele"), attr(swap, "ele"))) {
    .cli_action("Internal error on an ele to ele swap-out.",
      action = "abort",
      .internal = TRUE
    )
  }
  return(NULL)
}