#' .parse_tablo function
#'
#' This function parses a Tablo file (.tab), extracting key components and
#' optionally exporting the parsed data. It handles comments, retains key
#' statements, and processes the data to identify types, qualifiers, and other
#' attributes.
#'
#' @param tab_file The path to the Tablo file to be parsed.
#'
#' @importFrom tibble tibble
#' @importFrom purrr map2
#' @return A list containing the original Tablo data, the extracted components,
#'   and the file name.
#' @keywords internal
#' @noRd
.parse_tablo <- function(tab_file) {
  # to do:
  # extract all extract components from tablo file (currently compatible with
  # v62 and v7) Will break Tablo file down into all statements "kStatements"
  # plus additional components used throughout the wrapper Tablo<->LaTeX
  # conversions using powerful R functionalities, see:
  # https://adv-r.hadley.nz/translation.html?q=sql#introduction-20 Users will be
  # able to write either Tablo or LaTeX code for the actual model file, and
  # output LaTeX models seamlessly
  # ############################################################################

  # check whether user-provided Tablo, and if exists else retrieve internal
  if (grepl(pattern = "\\.tab", tab_file)) {
      tab <- readChar(
        con = tab_file,
        nchars = file.info(tab_file)[["size"]]
      )
  } else {
    tab <- internal_tab[[tab_file]]
    tab_file <- paste0(tab_file, ".tab")
  }

  # remove comments
  n_comments <- paste(unlist(x = strsplit(x = tab, "![^!]*!", perl = TRUE)), collapse = "")

  # key functions (all lines with these are retained)
  kStatements <- c(
    "File", "Coefficient", "Read", "Update", "Set", "Subset", "Formula",
    "Assertion", "Variable", "Equation", "Write", "Zerodivide"
  )

  # break into commands
  statements <- unlist(x = strsplit(x = n_comments, split = ";", perl = TRUE))

  # remove new lines and carriage returns
  statements <- gsub(pattern = "\r|\n", "", statements, perl = TRUE)
  statements <- statements[statements != ""]
  statements <- trimws(statements)

  extract <- tibble::tibble(
    type = sapply(X = strsplit(x = statements, split = " ", perl = TRUE), "[[", 1),
    remainder = trimws(
      sapply(
        X =
          strsplit(x = statements, split = " ", perl = TRUE),
        function(lineElem) {
          paste(lineElem[-1], collapse = " ")
        }
      )
    )
  )

  # use grepl w/ ignore case below
  # type for implicit
  for (r in 1:nrow(x = extract)) {
    if (!grepl(pattern = paste(kStatements, collapse = "|"), extract[r, "type"], ignore.case = TRUE)) {
      extract[r, "remainder"] <- paste(extract[r, "type"], extract[r, "remainder"])
      extract[r, "type"] <- extract[r - 1, "type"]
    }
  }

  tablo <- list(tab_file = tab_file,
                extract = extract,
                tab = tab)
  return(tablo)

  # will finish this later
  # Zerodivide statements ######################################################
  #zd <- subset(x = extract, subset = {tolower(x = type) == "zerodivide"})

  # zero_by_zero/nonzero specification
  # extract[["qualifier_arg"]] <- ifelse(extract[["type"]] == "Zerodivide",
  #   extract[["remainder"]],
  #   extract[["qualifier_arg"]]
  # )
  #
  # # Zerodivides done
  # extract[extract[["type"]] == "Zerodivide", "remainder"] <- NA
  #
  # # pull names from coeff, var, equations
  # extract[["name"]] <- unlist(x = purrr::map2(
  #   extract[["type"]],
  #   extract[["remainder"]],
  #   function(t, r) {
  #     if (t == "Equation") {
  #       sapply(X = strsplit(x = r, " "), "[[", 1)
  #     } else if (t %in% c("Coefficient", "Variable")) {
  #       spt <- unlist(x = strsplit(x = r, "\\s+"))
  #       l <- length(x = spt)
  #       spt[l]
  #     } else {
  #       NA
  #     }
  #   }
  # ))
  #
  # # remove names from remainder
  # extract[["remainder"]] <- unlist(x = purrr::map2(
  #   extract[["remainder"]],
  #   extract[["name"]],
  #   function(r, n) {
  #     if (!is.na(x = n) && r != n) {
  #       return(trimws(sub(pattern = n, "", r, fixed = TRUE)))
  #     } else {
  #       return(r)
  #     }
  #   }
  # ))
  #
  # extract[["maths"]] <- unlist(x = purrr::map2(
  #   extract[["type"]],
  #   extract[["remainder"]],
  #   function(t, r) {
  #     if (t %in% c("Formula", "Equation", "Update") & substr(r, 1, 1) == "(") {
  #       CleaveMath(input_string = r, rtn = "maths")
  #     } else {
  #       NA
  #     }
  #   }
  # ))
  #
  # extract[["remainder"]] <- unlist(x = purrr::map2(
  #   extract[["type"]],
  #   extract[["remainder"]],
  #   function(t, r) {
  #     if (t %in% c("Formula", "Equation", "Update") & substr(r, 1, 1) == "(") {
  #       CleaveMath(input_string = r, rtn = "sets")
  #     } else {
  #       r
  #     }
  #   }
  # ))
  #
  # # set, ss, read, write
  # extract[["name"]] <- trimws(ifelse(
  #   extract[["type"]] == "Set" & grepl(pattern = "=", extract[["remainder"]]),
  #   unlist(x = lapply(X = strsplit(x = extract[["remainder"]], "="), function(x) {
  #     x[1]
  #   })),
  #   ifelse(
  #     extract[["type"]] == "Set" & grepl(pattern = "\\(", extract[["remainder"]]) & isFALSE(extract[["intertemporal"]]),
  #     unlist(x = lapply(X = strsplit(x = extract[["remainder"]], "\\("), function(x) {
  #       x[1]
  #     })),
  #     ifelse(
  #       extract[["type"]] %in% c("Set", "Subset", "Read", "Write"),
  #       unlist(x = lapply(X = strsplit(x = extract[["remainder"]], " "), function(x) {
  #         x[1]
  #       })),
  #       extract[["name"]]
  #     )
  #   )
  # ))
  #
  # extract[["definition"]] <- trimws(ifelse(extract[["type"]] == "Set" & grepl(pattern = "\\(", extract[["remainder"]]),
  #   sub(pattern = ")", "", unlist(x = lapply(X = strsplit(x = extract[["remainder"]], "\\("), function(x) {
  #     x[2]
  #   }))),
  #   ifelse(extract[["type"]] == "Set" & grepl(pattern = "=", extract[["remainder"]]),
  #     unlist(x = lapply(X = strsplit(x = extract[["remainder"]], "="), function(x) {
  #       x[2]
  #     })),
  #     NA
  #   )
  # ))
  #
  # extract[["definition"]] <- unlist(x = purrr::pmap(
  #   list(
  #     extract[["type"]],
  #     extract[["name"]],
  #     extract[["remainder"]],
  #     extract[["definition"]]
  #   ),
  #   function(t, n, r, d) {
  #     if (t %in% c("Set", "Subset") & is.na(x = d)) {
  #       trimws(sub(pattern = n, "", r))
  #     } else {
  #       d
  #     }
  #   }
  # ))
  #
  # extract[extract[["type"]] %in% c("Set", "Subset"), "remainder"] <- NA
  #
  # # File and read
  # extract[extract[["type"]] == "File", "name"] <- extract[extract[["type"]] == "File", "remainder"]
  # extract[extract[["type"]] == "File", "remainder"] <- NA
  #
  # extract[["name"]] <- ifelse(extract[["type"]] == "Read",
  #   unlist(x = sapply(X = strsplit(x = extract[["remainder"]], " "), "[[", 1)),
  #   extract[["name"]]
  # )
  #
  # extract[["definition"]] <- ifelse(extract[["type"]] == "Read",
  #   unlist(x = sapply(X = strsplit(x = extract[["remainder"]], " "), function(x) {
  #     paste(x[-1], collapse = " ")
  #   })),
  #   extract[["definition"]]
  # )
  #
  # extract[extract[["type"]] == "Read", "remainder"] <- NA
  #
  # # coefficients and variables with no (NA) sets
  # extract[["remainder"]] <- ifelse(!grepl(pattern = "\\(", extract[["remainder"]]) & extract[["type"]] %in% c("Variable", "Coefficient"),
  #   NA,
  #   extract[["remainder"]]
  # )
  #
  #
  # # equations without sets
  # extract[["maths"]] <- ifelse(is.na(x = extract[["maths"]]) & extract[["type"]] %in% c("Equation", "Formula"),
  #   extract[["remainder"]],
  #   extract[["maths"]]
  # )
  #
  # extract[["remainder"]] <- ifelse(substr(extract[["remainder"]], 1, 1) != "(",
  #   NA,
  #   extract[["remainder"]]
  # )
  #
  # extract[["remainder"]] <- sub(pattern = "\\s+", " ", extract[["remainder"]])
  # extract[["remainder"]] <- sub(pattern = "\\)\\s+\\(", "\\)\\(", extract[["remainder"]])
  #
  # extract[["remainder"]] <- ifelse(extract[["type"]] %in% c("Variable", "Coefficient", "Formula", "Equation") & is.na(x = extract[["remainder"]]),
  #   "null_set",
  #   extract[["remainder"]]
  # )
  #
  # colnames(extract)[colnames(extract) == "remainder"] <- "full_set"
  #
  #
  #
  # )
  #
  # # get components of Read and Set statements
  # extract[["qualifier"]] <- unlist(x = ifelse(extract[["type"]] %in% c("Read", "Set"),
  #   lapply(X = strsplit(x = extract[["definition"]], "from file "), function(r) {
  #     sapply(X = strsplit(x = r[2], " "), "[[", 1)
  #   }),
  #   extract[["qualifier"]]
  # ))
  #
  # extract[["qualifier_arg"]] <- unlist(x = ifelse(extract[["type"]] %in% c("Read", "Set"),
  #   lapply(X = strsplit(x = extract[["definition"]], "header"), function(r) {
  #     gsub(pattern = '"', "", trimws(r[2]))
  #   }),
  #   extract[["qualifier_arg"]]
  # ))
  #
  # # get information from coefficients and match to read statements
  # coeff <- extract[extract[["type"]] == "Coefficient", ]
  # r_idx <- match(
  #   unlist(x = extract[extract[["type"]] == "Read", "name"]),
  #   coeff[["name"]]
  # )
  #
  # extract[extract[["type"]] == "Read", "information"] <- coeff[["information"]][r_idx]
  #
  # # split explicit character vectors
  # extract[["definition"]] <- ifelse(grepl(pattern = ",", extract[["definition"]]),
  #   strsplit(x = extract[["definition"]], ","),
  #   extract[["definition"]]
  # )
  #
  # # convert list of lists to list of vectors
  # extract[["definition"]] <- lapply(X = extract[["definition"]], unlist)
  # # drop SUPsub (no application)
  # extract <- subset(x = extract, select = -SUPsub)
  #
  # # identify headers that are read in under a different name
  #
  #
  # if (!is.null(x = export_dir)) {
  #   data.table::fwrite(x = extract, export_dir, sep = "\t")
  # }
  #

}
