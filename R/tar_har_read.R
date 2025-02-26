#' Read a GEMPACK HAR File into R List of Arrays
#'
#' Reads in a GEMPACK HAR file and returns its representation as a list.
#' Currently, it can only process integer headers, real full headers, and
#' character headers.
#'
#' @inheritParams teems_base
#'
#' @param con Character string. Path to a GTAP HAR file.
#'
#' @details Function modified from
#'   https://rdrr.io/github/USDA-ERS/MTED-HARr/src/R/read_har.r
#'
#' @importFrom purrr pluck
#' @return A list of arrays corresponding to GTAP database headers
#' @keywords internal
#' @noRd
.read_har <- function(con,
                      data_type,
                      header_rename,
                      coefficient_rename,
                      full_exclude = NULL,
                      append = NULL) {

  # map connection to data type (GTAP database file naming is inconsistent across releases)
  implied_data_type <- .har_match(con = con)

  if (!identical(x = data_type, y = implied_data_type)) {
    stop("got the wrong file here buster")
  }
  # Open the file
  if (is.character(x = con)) {
    con <- file(con, "rb")
  }

  # Read all bytes into a vector
  cf <- raw()
  while (length(x = a <- readBin(con, raw(), n = 1e9)) > 0) {
    cf <- c(cf, a)
  }

  # Read until you hit the end of the file
  while (length(x = charRead <- readBin(con, raw())) > 0) {
    cf <- c(cf, charRead)
  }
  
  har_file <- basename(path = summary(object = con)[["description"]])

  # Close the file
  close(con)

  if (cf[1] == 0xfd) {
    currentHeader <- ""
    headers <- list()
    i <- 2
    while (i < length(x = cf)) {
      # read the first byte
      fb <- cf[i]
      i <- i + 1
      bitsLength <- as.integer(x = rawToBits(fb))[3:8]
      toRead <- as.integer(x = rawToBits(fb))[1:2]
      toReadBytes <- Reduce(function(a, f) {
        a <- a + 2^(f - 1) * toRead[f]
      }, 1:length(x = toRead), 0)

      if (toReadBytes > 0) {
        for (i in (i):(i + toReadBytes - 1)) {
          bitsLength <- c(bitsLength, rawToBits(cf[i]))
        }
        i <- i + 1
      }

      recordLength <- Reduce(
        function(a, f) {
          a <- a + 2^(f - 1) * bitsLength[f]
        },
        1:length(x = bitsLength),
        0
      )

      if (recordLength == 4) {
        currentHeader <- trimws(rawToChar(cf[(i):(i + recordLength - 1)]))
      }
      if (is.null(x = headers[[currentHeader]])) {
        headers[[currentHeader]] <- list()
      }

      if (is.null(x = headers[[currentHeader]]$records)) {
        headers[[currentHeader]]$records <- list()
      }

      headers[[currentHeader]]$records[[length(x = headers[[currentHeader]]$records) +
        1]] <- cf[(i):(i + recordLength - 1)]
      i <- i + recordLength
      totalLength <- recordLength + 1 + toReadBytes
      endingBits <- intToBits(totalLength)
      maxPosition <- max(which(x = endingBits == 1))

      if (maxPosition <= 6) {
        needEnd <- 0
      } else {
        needEnd <- 0 + ceiling((maxPosition - 6) / 8)
      }

      expectedEnd <- packBits(c(intToBits(needEnd)[1:2], intToBits(totalLength))[1:(8 *
        (needEnd + 1))], "raw")
      expectedEnd <- expectedEnd[length(x = expectedEnd):1]

      if (any(cf[i:(i + length(x = expectedEnd) - 1)] != expectedEnd)) {
        stop("Surprising end of record")
      }

      i <- i + length(x = expectedEnd)
    }
  } else {
    headers <- list()
    i <- 1
    while (i < length(x = cf)) {
      # Read the length of the record
      toRead <- readBin(cf[i:(i + 3)], "integer", size = 4)
      if (toRead == 4) {
        if (!all(cf[(i + 4):(i + 3 + toRead)] == 0x20)) {
          headers[[trimws(rawToChar(cf[(i + 4):(i + 3 + toRead)]))]] <- list(
            start =
              i
          )
        }
      }
      i <- i + 3 + toRead + 1
      hasRead <- readBin(cf[i:(i + 3)], "integer", size = 4)
      if (hasRead != toRead) {
        warning(paste("A broken record", i, hasRead, toRead))
      }
      i <- i + 4
    }

    for (h in 1:length(x = headers)) {
      headers[[h]]$binary <- cf[headers[[h]]$start:ifelse(h < length(x = headers), headers[[h +
        1]]$start - 1, length(x = cf))]
    }

    # Separate records
    for (h in names(headers)) {
      headers[[h]]$records <- list()

      i <- 1

      while (i < length(x = headers[[h]]$binary)) {
        toRead <- readBin(headers[[h]]$binary[i:(i + 3)], "integer", size = 4)
        i <- i + 4
        headers[[h]]$records[[length(x = headers[[h]]$records) + 1]] <- readBin(headers[[h]]$binary[i:(i +
          toRead - 1)], raw(), n = toRead)
        i <- i + toRead
        hasRead <- readBin(headers[[h]]$binary[i:(i + 3)], "integer",
          size =
            4
        )
        i <- i + 4
        if (toRead != hasRead) {
          warning(paste("toRead different from hasRead in ", h))
        }
      }
    }
  }

  # Process first and second records
  for (h in names(headers)) {
    headers[[h]]$header_name <- trimws(rawToChar(headers[[h]]$records[[1]][1:4]))
    headers[[h]]$type <- rawToChar(headers[[h]]$records[[2]][5:10])
    headers[[h]]$information <- trimws(rawToChar(headers[[h]]$records[[2]][11:80]))
    headers[[h]]$numberOfDimensions <- readBin(headers[[h]]$records[[2]][81:84], "integer",
      size =
        4
    )

    headers[[h]]$dimensions <- c()

    for (i in 1:headers[[h]]$numberOfDimensions) {
      headers[[h]]$dimensions <- c(
        headers[[h]]$dimensions,
        readBin(headers[[h]]$records[[2]][(85 + (i - 1) * 4):(85 + i * 4)], "integer",
          size =
            4
        )
      )
    }
  }

  # Process character headers 1CFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == "1CFULL") {
      contents <- Reduce(
        function(a, f) {
          c(a, headers[[h]]$records[[f]][17:length(x = headers[[h]]$records[[f]])])
        },
        3:length(x = headers[[h]]$records),
        c()
      )

      contents[contents == 0x00] <- as.raw(0x20)

      m <- matrix(
        rawToChar(contents, multiple = TRUE),
        nrow =
          headers[[h]]$dimensions[[2]],
        ncol =
          headers[[h]]$dimensions[[1]]
      )

      # do not remove empty space in the history header
      # LREG in GTAP11 uses LATIN1 encoding
      if (tolower(x = h) == "xxhs") {
        toRet <- apply(m, 2, paste, collapse = "")
      } else if (h == "LREG") {
        toRet <- trimws(iconv(
          apply(m, 2, paste, collapse = ""),
          from = "latin1",
          to = "UTF-8"
        ))
      } else {
        toRet <- trimws(apply(m, 2, paste, collapse = ""))
      }

      headers[[h]]$data <- toRet
    }
  }

  # Process character headers 2IFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == "2IFULL") {
      m <- matrix(
        readBin(
          Reduce(
            function(a, f) {
              c(a, headers[[h]]$records[[f]][33:length(x = headers[[h]]$records[[f]])])
            },
            3:length(x = headers[[h]]$records),
            c()
          ),
          "integer",
          size = 4,
          n = prod(headers[[h]]$dimensions)
        ),
        nrow =
          headers[[h]]$dimensions[[1]],
        ncol =
          headers[[h]]$dimensions[[2]]
      )
      headers[[h]]$data <- m
    }
  }

  # Process real headers 2RFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == "2RFULL") {
      m <- array(
        readBin(
          Reduce(
            function(a, f) {
              c(a, headers[[h]]$records[[f]][33:length(x = headers[[h]]$records[[f]])])
            },
            3:length(x = headers[[h]]$records),
            c()
          ),
          "double",
          size = 4,
          n = prod(headers[[h]]$dimensions)
        ),
        dim = headers[[h]]$dimensions
      )
      headers[[h]]$data <- m
    }
  }

  # Process real  headers REFULL
  for (h in names(headers)) {
    if (headers[[h]]$type %in% c("REFULL", "RESPSE")) {
      # Get used dimensions and their names from record 3
      headers[[h]]$definedDimensions <- readBin(headers[[h]]$records[[3]][5:8], "integer",
        size =
          4
      )
      headers[[h]]$usedDimensions <- readBin(headers[[h]]$records[[3]][13:16], "integer",
        size =
          4
      )
      headers[[h]]$coefficient <- trimws(rawToChar(headers[[h]]$records[[3]][17:28]))

      if (headers[[h]]$usedDimensions > 0) {
        m <- matrix(
          strsplit(x = rawToChar(headers[[h]]$records[[3]][33:(33 + headers[[h]]$usedDimensions *
            12 - 1)]), "")[[1]],
          nrow =
            12,
          ncol =
            headers[[h]]$usedDimensions
        )

        dnames <- apply(m, 2, paste, collapse = "")
        dimNames <- Map(function(f) {
          NULL
        }, 1:headers[[h]]$usedDimensions)
        actualDimsNamesFlags <- headers[[h]]$records[[3]][(33 + headers[[h]]$usedDimensions *
          12) + 0:6]
        actualDimsNames <- ifelse(actualDimsNamesFlags == 0x6b, TRUE, FALSE)
        uniqueDimNames <- unique(x = dnames[actualDimsNames])

        if (length(x = uniqueDimNames) > 0) {
          for (d in 1:length(x = uniqueDimNames)) {
            nele <- readBin(headers[[h]]$records[[3 + d]][13:16], "integer", size = 4)

            m <- matrix(
              strsplit(x = rawToChar(headers[[h]]$records[[3 + d]][17:(17 +
                nele * 12 - 1)]), "")[[1]],
              nrow =
                12,
              ncol =
                nele
            )

            for (dd in which(x = dnames == uniqueDimNames[d])) {
              dimNames[[dd]] <- trimws(apply(m, 2, paste, collapse = ""))
              # Add dimension name
              names(dimNames)[dd] <- trimws(uniqueDimNames[d])
            }
          }
        }

        dataStart <- 3 + length(x = uniqueDimNames) + 1

        if (headers[[h]]$type == "REFULL") {
          numberOfFrames <- readBin(headers[[h]]$records[[dataStart]][5:8], "integer")
          numberOfDataFrames <- (numberOfFrames - 1) / 2
          dataFrames <- (dataStart) + 1:numberOfDataFrames * 2
          dataBytes <- do.call(what = c, Map(function(f) {
            headers[[h]]$records[[f]][9:length(x = headers[[h]]$records[[f]])]
          }, dataFrames))

          m <- array(
            readBin(
              dataBytes,
              "double",
              size = 4,
              n = prod(headers[[h]]$dimensions)
            ),
            dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
            dimnames = dimNames
          )
        } else {
          elements <- readBin(headers[[h]]$records[[dataStart]][5:8], "integer",
            size =
              4
          )
          dataVector <- rep(x = 0, prod(headers[[h]]$dimensions))

          for (rr in (dataStart + 1):length(x = headers[[h]]$records)) {
            dataBytes <- headers[[h]]$records[[rr]][17:length(x = headers[[h]]$records[[rr]])]

            currentPoints <- length(x = dataBytes) / 8

            locations <- readBin(dataBytes[1:(4 * currentPoints)],
              "integer",
              size = 4,
              n = currentPoints
            )
            values <- readBin(dataBytes[(4 * currentPoints + 1):(8 * currentPoints)],
              "double",
              size = 4,
              n = currentPoints
            )

            dataVector[locations] <- values
          }

          m <- array(dataVector,
            dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
            dimnames = dimNames
          )
        }
      } else {
        m <- array(
          readBin(
            headers[[h]]$records[[length(x = headers[[h]]$records)]][9:length(x = headers[[h]]$records[[3]])],
            "double",
            size = 4,
            n = prod(headers[[h]]$dimensions)
          ),
          dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions]
        )
      }

      headers[[h]]$data <- m
    }
    
    # records missing for the following parameter headers
    if (identical(x = data_type, y = "par")) {
      if (identical(x = purrr::pluck(headers, h, "header_name"), y = "RDLT")) {
        purrr::pluck(headers, h, "coefficient") <- "RORDELTA"
      }

      if (identical(x = purrr::pluck(headers, h, "header_name"), y = "SLUG")) {
        purrr::pluck(headers, h, "coefficient") <- "SLUG"
      }
    }
  }

  # renaming headers if header_rename != NULL
  if (!is.null(x = header_rename)) {
    if (!all(is.element(el = names(x = header_rename), set = names(x = headers)))) {
      errant_headers <- names(x = header_rename)[!is.element(el = names(x = header_rename),
                                                             set = names(x = headers))]
      stop(paste("The header(s) specified for renaming:", errant_headers, "is(are) not found in the HAR file."))
    }

    r_idx <- match(x = names(x = header_rename), table = names(x = headers))
    names(x = headers)[r_idx] <- header_rename

    for (nme in seq_along(header_rename)) {
      h <- header_rename[[nme]]
      purrr::pluck(.x = headers, h, "header_name") <- h
    }
  }

  # renaming coefficients if coefficient_rename != NULL
  if (!is.null(x = coefficient_rename)) {
    coefficient_names <- lapply(headers, function(h) {
      h[["coefficient"]]
    })
    if (!all(is.element(el = names(x = coefficient_rename), set = coefficient_names))) {
      errant_headers <- names(x = coefficient_rename)[!is.element(
        el = names(x = coefficient_rename),
        set = coefficient_names
      )]
      stop(paste("The header(s) specified for renaming:", errant_headers, "is(are) not found in the HAR file."))
    }

    for (nme in seq_along(coefficient_rename)) {
      old_coeff <- names(x = coefficient_rename[nme])
      new_coeff <- coefficient_rename[[nme]]
      r_idx <- match(x = old_coeff, table = coefficient_names)
      purrr::pluck(.x = headers, r_idx, "coefficient") <- new_coeff
    }
  }

  # drop records and unnecessary data
  headers <- lapply(X = headers,
                    FUN = function(h) {
                      h <- h[!is.element(el = names(x = h), set = c("start", "binary", "records"))]
                      return(h)
                    })

  if (!is.null(x = append)) {
    headers <- c(headers, append)
  }
  
  # set type and aggregate
  headers <- lapply(X = headers,
                    FUN = function(h) {
                      if (is.element(el = h[["type"]], set = c("1CFULL", "2IFULL"))) {
                        h[["aggregate"]] <- FALSE
                      } else if (!isFALSE(x = h[["aggregate"]])) {
                        h[["aggregate"]] <- TRUE
                      }
                      return(h)
                    })
  
  # GDYN database uses natres instead of natlres for whatever reason (support email submitted)
  # temporarily disable GDYN runs
  if (identical(x = har_file, y = "gdset.har")) {
    if (any(grepl(pattern = "NatRes", purrr::pluck(.x = headers, "ENDW", "data")))) {
      stop("GDYN 11c database temporarily incompatible due to NatRes/NatlRes mismatch in set file.")
    }
  }
  

# 1CFULL = character
# 2IFULL = integer
# REFULL = real

  # full exclude
  if (!is.null(x = full_exclude)) {
    headers <- headers[!is.element(el = names(x = headers), set = full_exclude)]
  }

  return(headers)
}
