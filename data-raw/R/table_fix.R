.table_fix <- function(single = NULL,
                       double = NULL,
                       trebble = NULL,
                       table,
                       prefix,
                       data_type) {
  if (data_type == "par") {
  descr <- table[,4][[1]]
  } else if (data_type == "dat") {
    descr <- table[,3][[1]]
  }

  fixed_descr <- list()
  for (line in seq_along(descr)) {
    if (line %in% single) {
      fixed_descr[line] <- descr[line]
    } else if (line %in% double) {
      fixed_descr[line] <- sub("- ", "", paste(descr[line], descr[line+1]))
    } else if (line %in% trebble) {
      fixed_descr[line] <- sub("- ", "", paste(descr[line], descr[line+1], descr[line+2]))
    } else {
      fixed_descr[line] <- NA
    }
  }

  fixed_descr <- unlist(fixed_descr)[!is.na(x = unlist(fixed_descr))]

  if (data_type == "par") {
    table <- table[!is.na(table[, 1]), ]
    table[, 4] <- fixed_descr
    table[, 3] <- lapply(table[, 3], strsplit, split = "\\*")
    colnames(x = table) <- c("idx", paste0(prefix, c("header", "set", "description")))
    # if (prefix == "v62") {
    #   table <- rbind(table,
    #                  tibble::tibble(
    #                    v62header = rep(NA, NA_extend),
    #                    v62set = rep(NA, NA_extend),
    #                    v62description = rep(NA, NA_extend)
    #                  ))
    # }
  } else if (data_type == "dat") {
    table <- tibble::as_tibble(table)
    table <- table[!is.na(table[, 1]), ]
    table[, 3] <- fixed_descr
    colnames(x = table) <- c("idx", paste0(prefix, c("header", "description")))
    # if (prefix == "v62") {
    #   table <- rbind(table,
    #                  tibble::tibble(
    #                    v62header = rep(NA, NA_extend),
    #                    v62description = rep(NA, NA_extend)
    #                  ))
    # }
  }




  return(table)
}
