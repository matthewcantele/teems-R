#' @importFrom rlang is_integerish
#' @importFrom data.table data.table setnames setkeyv
#' @importFrom purrr map2 map_chr
#' 
#' @keywords internal
#' @noRd
.finalize_data <- function(data,
                           sets,
                           model,
                           write_dir,
                           call) {
  data <- data[names(data) %in% subset(
    model, type == "Coefficient" & !is.na(file),
    header,
    1
  )]

  # data <- lapply(data,
  #                .custom_mod,
  #                sets = sets)
  if (any(grepl("\\(intertemporal\\)", sets$qualifier_list))) {
    int_sets <- subset(
      sets,
      grepl("\\(intertemporal\\)", sets$qualifier_list),
      ele,
      1
    )

    l_idx <- match(names(data), model$header)
    data <- purrr::map2(
      data,
      l_idx,
      function(dt, id) {
        full_sets <- model$ls_upper_idx[[id]]
        if (any(duplicated(full_sets))) {
          full_sets[duplicated(full_sets)] <- attr(dt, "sorted")[which(duplicated(full_sets))]
        }
        if (any(names(int_sets) %in% full_sets)) {
          int_set <- intersect(names(int_sets), full_sets)
          nonint_set <- full_sets[!full_sets %in% int_set]
          dt <- dt[, .(with(int_sets, get(int_set)), Value), by = nonint_set]
          data.table::setnames(dt, new = c(full_sets, "Value"))
          data.table::setkeyv(dt, cols = full_sets)
        }
        return(dt)
      }
    )

    n_timestep_header <- .o_n_timestep_header()
    timestep_header <- .o_timestep_header()
    n_timestep <- data.table::data.table(Value = length(attr(sets, "time_steps")))
    class(n_timestep) <- c(.o_n_timestep_header(), "dat", class(n_timestep))

    t_header_set <- purrr::pluck(model, "ls_upper_idx", timestep_header)
    timesteps <- data.table::data.table(
      with(sets$ele, get(t_header_set)),
      attr(sets, "time_steps")
    )
    class(timesteps) <- c(timestep_header, "par", class(timesteps))
    data.table::setnames(timesteps, c(t_header_set, "Value"))
    data.table::setkeyv(timesteps, cols = t_header_set)
    append <- list(n_timestep, timesteps)
    names(append) <- purrr::map_chr(append, function(c) {
      class(c)[[1]]
    })
    data <- c(data, append)
  }

  data <- lapply(data, function(dt) {
    if (!colnames(dt) %=% "Value") {
      dt_col <- gsub("\\.[0-9]+", "", colnames(dt))
      dt_sets <- with(sets$ele, mget(dt_col[!dt_col %in% "Value"]))
      expected <- as.integer(prod(lengths(dt_sets)))
      attr(dt, "dim_sizes") <- lengths(dt_sets)
      attr(dt, "dimen") <- paste(lengths(dt_sets), collapse = " ")
    } else {
      expected <- 1L
      attr(dt, "dim_sizes") <- expected
      attr(dt, "dimen") <- as.character(expected)
    }

    if (!expected %=% nrow(dt)) {
      browser()
      .cli_action(data_err$data_set_mismatch,
        action = "abort",
        call = call
      )
    }
    return(dt)
  })

  r_idx <- match(names(data), model$header)
  ndigits <- .o_ndigits()

  data <- purrr::map2(
    data,
    r_idx,
    function(dt, id) {
      attr(dt, "file") <- model$file[id]
      if (!grepl("integer", model$qualifier_list[id])) {
        type <- "Real"
      } else {
        type <- "Integer"
      }

      if (type %=% "Real" && !rlang::is_integerish(dt$Value)) {
        dt[, Value := format(
          round(Value, ndigits),
          trim = TRUE,
          nsmall = ndigits,
          scientific = FALSE
        )]
      } else {
        dt[, Value := as.integer(Value)]
      }

      lead <- paste(
        attr(dt, "dimen"),
        type,
        "SpreadSheet Header",
        paste0('"', class(dt)[1], '"'),
        "LongName",
        paste0('"', model$label[id], '";')
      )

      attr(dt, "lead") <- lead
      return(dt)
    }
  )

  sets <- sets[sets$name %in% subset(
    model, type == "Set" & !is.na(file),
    name,
    1
  ), "ele"][[1]]

  r_idx <- match(names(sets), model$name)
  sets <- purrr::map2(
    sets,
    r_idx,
    function(s, id) {
      lead <- paste(
        length(s),
        "Strings Length",
        max(purrr::map_int(s, nchar)),
        "Header",
        paste0("\"", model$header[id], "\""),
        "LongName",
        paste0('"', model$label[id], '";')
      )
      attr(s, "lead") <- lead
      attr(s, "file") <- model$file[id]
      class(s) <- c("set", class(s))
      return(s)
    }
  )

  data <- c(data, sets)
  return(data)
}