#' @importFrom rlang try_fetch
#' @importFrom purrr map2
#' @importFrom tools file_ext
#'
#'
#' @keywords internal
#' @noRd
.check_set_mappings <- function(args_list,
                                database_version,
                                call,
                                envir,
                                quiet) {
  map_args <- names(x = args_list)[grepl("_mapping", names(x = args_list))]
  mappings <- mget(x = map_args, envir = envir)

  if (is.element(el = database_version, set = c("v9", "v10"))) {
    endowment_set <- "ENDW_COMM"
    region_set <- "REG"
    tradables_set <- "TRAD_COMM"
  } else if (identical(x = database_version, y = "v11")) {
    endowment_set <- "ENDW"
    region_set <- "REG"
    tradables_set <- "COMM"
  }

  ls_set_ele <- rlang::try_fetch(purrr::map2(
    .x = mappings,
    .y = names(x = mappings),
    .f = function(mapping, mapping_name)
    {
      if (identical(x = tolower(x = tools::file_ext(x = mapping)), y = "csv")) {
        .check_input_file(file = mapping,
                          ext = "csv",
                          call = call)
        if (identical(x = mapping_name, y = "endowment_mapping")) {
          map <- .set_ele_read(file = mapping,
                               col = 2,
                               set_name = mappings[["endowment_mapping"]])
        } else if (identical(x = mapping_name, y = "region_mapping")) {
          map <- .set_ele_read(file = mapping,
                               col = 2,
                               set_name = mappings[["region_mapping"]])
        } else if (identical(x = mapping_name, y = "sector_mapping")) {
          map <- .set_ele_read(file = mapping,
                               col = 2,
                               set_name = mappings[["sector_mapping"]])
        }
      } else {
        if (identical(x = mapping_name, y = "endowment_mapping")) {
          .check_internal_set_mapping(set = endowment_set,
                                      database_version = database_version,
                                      mapping = mapping,
                                      call = call)
        } else if (identical(x = mapping_name, y = "region_mapping")) {
          .check_internal_set_mapping(set = region_set,
                                      database_version = database_version,
                                      mapping = mapping,
                                      call = call)
        } else if (identical(x = mapping_name, y = "sector_mapping")) {
          .check_internal_set_mapping(set = tradables_set,
                                      database_version = database_version,
                                      mapping = mapping,
                                      call = call)
        }
      }
    }
  ))
  if (!quiet) {
    with(data = ls_set_ele, expr = {
      .set_ele_inform(set_ele = endowment_mapping,
                      set_name = "endowment",
                      model_set = endowment_set)
      .set_ele_inform(set_ele = region_mapping,
                      set_name = "region",
                      model_set = region_set)
      .set_ele_inform(set_ele = sector_mapping,
                      set_name = "sector",
                      model_set = tradables_set)
    })
  }
  set_files <- sapply(X = ls_set_ele, FUN = names)
  r_idx <- match(x = names(x = set_files), table = names(x = args_list))
  args_list[r_idx] <- set_files

return(args_list)
}



