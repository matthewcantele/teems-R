#' @keywords internal
#' @noRd
.check_set <- function(premodel,
                     postmodel) {
  postmodel[["setname"]] <- toupper(x = postmodel[["setname"]])

  # match pre and post ()
  r_idx <- match(
    x = premodel[["name"]],
    table = postmodel[["setname"]]
  )

  premodel[["post_ele"]] <- postmodel[["elements"]][r_idx]
  names(x = premodel[["post_ele"]]) <- premodel[["name"]]

  # coerce strings to numeric in post if possible
  premodel[["post_ele"]] <- lapply(X = premodel[["post_ele"]],
         FUN = function(set) {
           set <- ifelse(test = !is.na(x = suppressWarnings(expr = as.numeric(x = set))),
                  yes = as.numeric(x = set),
                  no = set)

           return(set)
         })

  # check that the sets and elements parsed from tablo code are identical
  # to the postmodel binary output
  if (!isTRUE(x = all.equal(target = premodel[["elements"]], current = premodel[["post_ele"]]))) {
    stop("Tablo-parsed sets and/or elements are not identical to post-model binary outputs")
  }
  return(invisible(NULL))
}
