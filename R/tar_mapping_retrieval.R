#' @importFrom rlang current_env
#' @importFrom purrr map2
#' @importFrom data.table fread setnames
#' @importFrom dplyr setdiff union intersect
#'
#' @keywords internal
#' @noRd
.retrieve_mappings <- function(set_map_file_inputs,
                               set_map_file_input_names,
                               sets,
                               margin_sectors = c("atp", "otp", "wtp"),
                               cgds_sector = "zcgds",
                               CYRS = NULL,
                               metadata,
                               set_hash) {

  list2env(metadata, rlang::current_env())
  names(set_map_file_inputs) <- set_map_file_input_names
  # merge all set mappings here (file inputs plus any manual entries - future ext)
  set_mappings <- set_map_file_inputs
  # read in set mappings
  core_sets <- purrr::map2(
    set_mappings,
    names(set_mappings),
    function(map_file, set_name) {
      mapping <- read.csv(map_file)
      # tolower and no spaces for user-provided (internal already vetted)
      mapping[, 2] <- gsub(" ", "_", mapping[, 2])
      mapping[, 2] <- tolower(mapping[, 2])

      colnames(mapping) <- c("origin", "mapping")
      return(mapping)
    }
  )

  names(core_sets) <- names(set_mappings)

  # margin mapping
  if (data_format %=% "v6.2") {
    core_sets$MARG_COMM <- subset(
      core_sets$TRAD_COMM,
      origin %in% margin_sectors
    )
    colnames(core_sets$MARG_COMM) <- c("origin", "mapping")
  } else if (data_format %=% "v7.0") {
    core_sets$MARG <- subset(
      core_sets$COMM,
      origin %in% margin_sectors
    )
    colnames(core_sets$MARG) <- c("origin", "mapping")
  }

  # intertemporal sets
  if (any(sets$intertemporal)) {
    int_sets <- purrr::pmap(
      list(
        sets$intertemporal,
        sets$name,
        sets$full_ele
      ),
      function(int, nme, ele) {
        if (int) {
          int_set <- data.frame(ele, ele)
          colnames(int_set) <- c("origin", "mapping")
          return(int_set)
        }
      }
    )

    int_sets <- purrr::compact(int_sets)
    names(int_sets) <- unlist(subset(
      sets,
      intertemporal,
      name
    ))
    core_sets <- c(core_sets, int_sets)
  }

  core_sets <- lapply(
    core_sets,
    function(df) {
      df <- df[order(df$mapping), ]
    }
  )

  r_idx <- match(sets$name, names(core_sets))

  sets$mapping <- core_sets[r_idx]
  sets$mapping <- purrr::pmap(
    list(
      sets$name,
      sets$full_ele,
      sets$mapping,
      sets$operator
    ),
    function(nme, ele, map, op) {
      if (is.null(map)) {
        if (is.na(op)) {
          map <- data.frame(ele, ele)
          colnames(map) <- c("origin", "mapping")
        }
      }
      return(map)
    }
  )

  sets$mapped_ele <- sapply(
    sets$mapping,
    FUN = function(m) {
      unique(m$mapping)
    }
  )

  # dependent sets algo (so coeff of any model set can be used)
  while (any(sapply(sets$mapped_ele, is.null))) {
    sets$mapped_ele <- purrr::pmap(
      list(
        sets$name,
        sets$mapped_ele,
        sets$operator,
        sets$comp1,
        sets$comp2
      ),
      function(nme, mapped_ele, op, c1, c2) {
        if (is.null(mapped_ele)) {
          x <- unlist(subset(
            sets,
            name %in% c1,
            mapped_ele
          ), use.names = FALSE)
          y <- unlist(subset(
            sets,
            name %in% c2,
            mapped_ele
          ), use.names = FALSE)
          if (any(is.null(x), is.null(y))) {
            return(NULL)
          }
          mapped_ele <- do.call(op, list(x, y))
        }
        return(mapped_ele)
      }
    )
  }

  names(sets$mapping) <- sets$name
  while (any(sapply(sets$mapping, is.null))) {
    sets$mapping <- purrr::pmap(
      list(
        sets$name,
        sets$mapping,
        sets$operator,
        sets$comp1,
        sets$comp2
      ),
      function(nme, map, op, c1, c2) {
        if (is.null(map)) {
          x <- subset(
            sets,
            name %in% c1,
            mapping
          )[[1]][[1]]
          y <- subset(
            sets,
            name %in% c2,
            mapping
          )[[1]][[1]]
          if (any(is.null(x), is.null(y))) {
            return(NULL)
          }
          colnames(x) <- c("origin", "mapping")
          colnames(y) <- c("origin", "mapping")
          if (op %in% c("setdiff", "union", "intersect")) {
            map <- do.call(get(op, envir = asNamespace("dplyr")),
              list(x, y)
            )
          } else if (op %=% "c") {
            map <- rbind(x, y)
          }
        }
        return(map)
      }
    )
  }

  sets <- sets[, c("type", "name", "label", "file", "header", "intertemporal", "data_type", "full_ele", "mapped_ele", "mapping")]
  names(sets$mapping) <- sets$name
  names(sets$mapped_ele) <- sets$name
  CYRS <- array2DF(CYRS$data + metadata$reference_year)
  attr(sets, "CYRS") <- CYRS
  return(sets)
}
