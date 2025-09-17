#' @details Function modified from
#'   https://rdrr.io/github/USDA-ERS/MTED-HARr/src/R/read_har.r
#'
#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
.read_har <- function(con,
                      data_format = NULL,
                      data_type = NULL,
                      attach_metadata = FALSE,
                      call = NULL) {
  # cut this down to essentials
  # Open the file
  if (is.character(con)) {
    con <- file(con, "rb")
  }

  # map connection to data type (GTAP database file naming is inconsistent across releases)
  full_har_path <- normalizePath(path = summary(object = con)[["description"]])

  # Read all bytes into a vector
  cf <- raw()
  while (length(a <- readBin(con, raw(), n = 1e9)) > 0) {
    cf <- c(cf, a)
  }

  # Read until you hit the end of the file
  while (length(charRead <- readBin(con, raw())) > 0) {
    cf <- c(cf, charRead)
  }

  # Close the file
  close(con)

  implied_data_type <- .har_match(con = full_har_path)
  har_file <- basename(path = full_har_path)

  if (cf[1] == 0xfd) {
    currentHeader <- ""
    headers <- list()
    i <- 2
    while (i < length(cf)) {
      # read the first byte
      fb <- cf[i]
      i <- i + 1
      bitsLength <- as.integer(rawToBits(fb))[3:8]
      toRead <- as.integer(rawToBits(fb))[1:2]
      toReadBytes <- Reduce(function(a, f) {
        a <- a + 2^(f - 1) * toRead[f]
      }, 1:length(toRead), 0)

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
        1:length(bitsLength),
        0
      )

      if (recordLength == 4) {
        currentHeader <- trimws(rawToChar(cf[(i):(i + recordLength - 1)]))
      }
      if (is.null(headers[[currentHeader]])) {
        headers[[currentHeader]] <- list()
      }

      if (is.null(headers[[currentHeader]]$records)) {
        headers[[currentHeader]]$records <- list()
      }

      headers[[currentHeader]]$records[[length(headers[[currentHeader]]$records) +
        1]] <- cf[(i):(i + recordLength - 1)]
      i <- i + recordLength
      totalLength <- recordLength + 1 + toReadBytes
      endingBits <- intToBits(totalLength)
      maxPosition <- max(which(endingBits == 1))

      if (maxPosition <= 6) {
        needEnd <- 0
      } else {
        needEnd <- 0 + ceiling((maxPosition - 6) / 8)
      }

      expectedEnd <- packBits(c(intToBits(needEnd)[1:2], intToBits(totalLength))[1:(8 *
        (needEnd + 1))], "raw")
      expectedEnd <- expectedEnd[length(expectedEnd):1]

      if (any(cf[i:(i + length(expectedEnd) - 1)] != expectedEnd)) {
        stop("Surprising end of record")
      }

      i <- i + length(expectedEnd)
    }
  } else {
    headers <- list()
    i <- 1
    while (i < length(cf)) {
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

    for (h in 1:length(headers)) {
      headers[[h]]$binary <- cf[headers[[h]]$start:ifelse(h < length(headers), headers[[h +
        1]]$start - 1, length(cf))]
    }

    # Separate records
    for (h in names(headers)) {
      headers[[h]]$records <- list()

      i <- 1

      while (i < length(headers[[h]]$binary)) {
        toRead <- readBin(headers[[h]]$binary[i:(i + 3)], "integer", size = 4)
        i <- i + 4
        headers[[h]]$records[[length(headers[[h]]$records) + 1]] <- readBin(headers[[h]]$binary[i:(i +
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
    headers[[h]]$header <- trimws(rawToChar(headers[[h]]$records[[1]][1:4]))
    headers[[h]]$type <- rawToChar(headers[[h]]$records[[2]][5:10])
    # headers[[h]]$label <- trimws(rawToChar(headers[[h]]$records[[2]][11:80]))
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
          c(a, headers[[h]]$records[[f]][17:length(headers[[h]]$records[[f]])])
        },
        3:length(headers[[h]]$records),
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
      if (tolower(h) == "xxhs") {
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
              c(a, headers[[h]]$records[[f]][33:length(headers[[h]]$records[[f]])])
            },
            3:length(headers[[h]]$records),
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
              c(a, headers[[h]]$records[[f]][33:length(headers[[h]]$records[[f]])])
            },
            3:length(headers[[h]]$records),
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

      if (headers[[h]]$usedDimensions > 0) {
        m <- matrix(
          strsplit(rawToChar(headers[[h]]$records[[3]][33:(33 + headers[[h]]$usedDimensions *
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
        uniqueDimNames <- unique(dnames[actualDimsNames])

        if (length(uniqueDimNames) > 0) {
          for (d in 1:length(uniqueDimNames)) {
            nele <- readBin(headers[[h]]$records[[3 + d]][13:16], "integer", size = 4)

            m <- matrix(
              strsplit(rawToChar(headers[[h]]$records[[3 + d]][17:(17 +
                nele * 12 - 1)]), "")[[1]],
              nrow =
                12,
              ncol =
                nele
            )

            for (dd in which(dnames == uniqueDimNames[d])) {
              dimNames[[dd]] <- trimws(apply(m, 2, paste, collapse = ""))
              # Add dimension name
              names(dimNames)[dd] <- trimws(uniqueDimNames[d])
            }
          }
        }

        dataStart <- 3 + length(uniqueDimNames) + 1

        if (headers[[h]]$type == "REFULL") {
          numberOfFrames <- readBin(headers[[h]]$records[[dataStart]][5:8], "integer")
          numberOfDataFrames <- (numberOfFrames - 1) / 2
          dataFrames <- (dataStart) + 1:numberOfDataFrames * 2
          dataBytes <- do.call(what = c, Map(function(f) {
            headers[[h]]$records[[f]][9:length(headers[[h]]$records[[f]])]
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
          dataVector <- rep(0, prod(headers[[h]]$dimensions))

          for (rr in (dataStart + 1):length(headers[[h]]$records)) {
            dataBytes <- headers[[h]]$records[[rr]][17:length(headers[[h]]$records[[rr]])]

            currentPoints <- length(dataBytes) / 8

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
            headers[[h]]$records[[length(headers[[h]]$records)]][9:length(headers[[h]]$records[[3]])],
            "double",
            size = 4,
            n = prod(headers[[h]]$dimensions)
          ),
          dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions]
        )
      }

      headers[[h]]$data <- m
    }
  }

  if (attach_metadata || implied_data_type %=% "set") {
    DREL <- purrr::pluck(headers, "DREL", "data")
    DVER <- purrr::pluck(headers, "DVER", "data")
    metadata <- .har_meta(
      DREL = DREL,
      DVER = DVER,
      data_type = implied_data_type
    )

    metadata[["full_database_version"]] <- metadata[["database_version"]]
    metadata[["database_version"]] <- gsub("(\\d.*?)[A-Za-z]", "\\1", metadata[["database_version"]])
  }


  # manually pull out set names for pre v11
  # no telling how robust this is
  if (implied_data_type %=% "set") {
    if (metadata$database_version %in% c("v9", "v10")) {
      purrr::pluck(headers, "H1", "name") <- trimws(rawToChar(headers$H1$records[[2]][14:19]))
      purrr::pluck(headers, "H2", "name") <- trimws(rawToChar(headers$H2$records[[2]][14:25]))
      purrr::pluck(headers, "H6", "name") <- trimws(rawToChar(headers$H6$records[[2]][14:25]))
      purrr::pluck(headers, "H9", "name") <- trimws(rawToChar(headers$H9$records[[2]][14:25]))
      purrr::pluck(headers, "MARG", "name") <- trimws(rawToChar(headers$MARG$records[[2]][14:25]))
    } else if (metadata$database_version %=% "v11") {
      headers <- lapply(headers, function(h) {
        h$name <- h$header
        return(h)
      })
    }
  }
  
  if (is.null(data_type)) {
    data_type <- implied_data_type
  }

  if (is.null(data_format)) {
    data_format <- metadata$data_format
  }

  headers <- lapply(
    headers,
    function(h) {
      header <- h$header
      name <- h$name
      h <- h$data
      if (!is.null(h)) {
        if (is.null(name)) {
          class(h) <- c(header, data_type, data_format, class(h))
        } else {
          class(h) <- c(header, name, data_type, data_format, class(h))
        }
      }
      return(h)
    }
  )

  class(headers) <- c(data_type, class(headers))
  if (attach_metadata) {
    attr(headers, "metadata") <- metadata
  }

  return(headers)
}