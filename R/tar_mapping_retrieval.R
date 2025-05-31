#' @importFrom purrr map2
#' @importFrom data.table fread setnames
#' @importFrom dplyr setdiff union intersect
#'
#' @keywords internal
#' @noRd
.retrieve_mappings <- function(set_map_file_inputs,
                               set_map_file_input_names,
                               sets,
                               database_version,
                               margin_sectors = c("atp", "otp", "wtp"),
                               cgds_sector = "zcgds",
                               data_format) {
  names(x = set_map_file_inputs) <- set_map_file_input_names
  # merge all set mappings here (file inputs plus any manual entries - future ext)
  set_mappings <- set_map_file_inputs
  # read in set mappings
  core_sets <- purrr::map2(
    .x = set_mappings,
    .y = names(x = set_mappings),
    .f = function(map_file, set_name) {
      mapping <- read.csv(file = map_file)
      # tolower and no spaces for user-provided (internal already vetted)
      mapping[, 2] <- gsub(pattern = " ", replacement = "_", x = mapping[, 2])
      mapping[, 2] <- tolower(x = mapping[, 2])

      expected_origin_map <- with(
        data = sets[["full_ele"]],
        expr = get(x = set_name)
      )

      if (!all(is.element(el = expected_origin_map, set = mapping[, 1]))) {
        missing_origin <- setdiff(x = expected_origin_map, y = mapping[, 1])
        .cli_action(
          msg = "The provided mapping for {.val {set_name}} is
                         missing {length(missing_origin)} entr{?y/ies}
                         corresponding to {.val {missing_origin}}.",
          action = "abort",
          call = call
        )
      }
      colnames(x = mapping) <- c("origin", "mapping")
      return(mapping)
    }
  )

  names(x = core_sets) <- names(x = set_mappings)

  # margin mapping
  if (identical(x = data_format, y = "v6.2")) {
    core_sets[["MARG_COMM"]] <- subset(
      x = core_sets[["TRAD_COMM"]],
      subset = {
        is.element(
          el = origin,
          set = margin_sectors
        )
      }
    )
    colnames(core_sets[["MARG_COMM"]]) <- c("origin", "mapping")
  } else if (identical(x = data_format, y = "v7.0")) {
    core_sets[["MARG"]] <- subset(
      x = core_sets[["COMM"]],
      subset = {
        is.element(el = origin, set = margin_sectors)
      }
    )
    colnames(core_sets[["MARG"]]) <- c("origin", "mapping")
  }

  # intertemporal sets
  if (any(sets[["intertemporal"]])) {
    int_sets <- purrr::pmap(
      .l = list(
        sets[["intertemporal"]],
        sets[["name"]],
        sets[["full_ele"]]
      ),
      .f = function(int, nme, ele) {
        if (int) {
          int_set <- data.frame(ele, ele)
          colnames(x = int_set) <- c("origin", "mapping")
          return(int_set)
        }
      }
    )

    int_sets <- purrr::compact(.x = int_sets)
    names(x = int_sets) <- unlist(x = subset(
      x = sets,
      subset = intertemporal,
      select = name
    ))
    core_sets <- c(core_sets, int_sets)
  }

  core_sets <- lapply(
    X = core_sets,
    FUN = function(df) {
      df <- df[order(df[["mapping"]]), ]
    }
  )

  r_idx <- match(x = sets[["name"]], table = names(x = core_sets))

  sets[["mapping"]] <- core_sets[r_idx]
  sets[["mapping"]] <- purrr::pmap(
    .l = list(
      sets[["name"]],
      sets[["full_ele"]],
      sets[["mapping"]],
      sets[["operator"]]
    ),
    .f = function(nme, ele, map, op) {
      if (is.null(x = map)) {
        if (is.na(x = op)) {
          map <- data.frame(ele, ele)
          colnames(x = map) <- c("origin", "mapping")
        }
      }
      return(map)
    }
  )

  sets[["mapped_ele"]] <- sapply(
    X = sets[["mapping"]],
    FUN = function(m) {
      unique(x = m[["mapping"]])
    }
  )

  # dependent sets algo (so coeff of any model set can be used)
  while (any(sapply(X = sets[["mapped_ele"]], FUN = is.null))) {
    sets[["mapped_ele"]] <- purrr::pmap(
      .l = list(
        sets[["name"]],
        sets[["mapped_ele"]],
        sets[["operator"]],
        sets[["comp1"]],
        sets[["comp2"]]
      ),
      .f = function(nme, mapped_ele, op, c1, c2) {
        if (is.null(x = mapped_ele)) {
          x <- unlist(x = subset(
            x = sets,
            subset = {
              is.element(el = name, set = c1)
            },
            select = mapped_ele
          ), use.names = FALSE)
          y <- unlist(x = subset(
            x = sets,
            subset = {
              is.element(el = name, set = c2)
            },
            select = mapped_ele
          ), use.names = FALSE)
          if (any(is.null(x = x), is.null(x = y))) {
            return(NULL)
          }
          mapped_ele <- do.call(what = op, args = list(x, y))
        }
        return(mapped_ele)
      }
    )
  }

  names(x = sets[["mapping"]]) <- sets[["name"]]
  while (any(sapply(X = sets[["mapping"]], FUN = is.null))) {
    sets[["mapping"]] <- purrr::pmap(
      .l = list(
        sets[["name"]],
        sets[["mapping"]],
        sets[["operator"]],
        sets[["comp1"]],
        sets[["comp2"]]
      ),
      .f = function(nme, map, op, c1, c2) {
        if (is.null(x = map)) {
          x <- subset(
            x = sets,
            subset = {
              is.element(el = name, set = c1)
            },
            select = mapping
          )[[1]][[1]]
          y <- subset(
            x = sets,
            subset = {
              is.element(el = name, set = c2)
            },
            select = mapping
          )[[1]][[1]]
          if (any(is.null(x = x), is.null(x = y))) {
            return(NULL)
          }
          colnames(x = x) <- c("origin", "mapping")
          colnames(x = y) <- c("origin", "mapping")
          if (is.element(el = op, set = c("setdiff", "union", "intersect"))) {
            map <- do.call(
              what = get(
                x = op,
                envir = asNamespace(ns = "dplyr")
              ),
              list(x, y)
            )
          } else if (identical(x = op, y = "c")) {
            map <- rbind(x, y)
          }
        }
        return(map)
      }
    )
  }

  sets <- sets[, c("type", "name", "label", "file", "header", "intertemporal", "data_type", "full_ele", "mapped_ele", "mapping")]
  names(x = sets[["mapping"]]) <- sets[["name"]]
  names(sets[["mapped_ele"]]) <- sets[["name"]]
  return(sets)
}
