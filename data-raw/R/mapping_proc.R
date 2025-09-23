# set mappings #################################################################
# mapping data by database version, set, and mapping
# Define the function to process the mappings
process_mappings <- function(mapping_files,
                             db_version,
                             data_format) {

  ls_mappings <- list()
  for (v in db_version) {
    for (d in data_format) {
      set_names <- unique(basename(dirname(grep(file.path(v, d), mapping_files, value = TRUE))))
      for (s in set_names) {
        set_mappings <- grep(file.path(v, d, s), mapping_files, value = TRUE)

        # Read the files into a list of data frames and rename columns
        dt_list <- lapply(
          X = set_mappings,
          FUN = function(file) {
            # Extract the mapping name from the file path
            mapping_name <- tools::file_path_sans_ext(x = basename(path = file))

            # Read the CSV file
            dt <- data.table::fread(input = file)

            # Rename the second column to the mapping name
            data.table::setnames(x = dt,
                                 old = "mapping",
                                 new = mapping_name)

            return(dt)
          }
        )

        # Merge all data frames in the list by the first column
        merged_dt <- Reduce(
          f = function(x, y) {
            merge(x, y, by = colnames(x = dt_list[[1]])[1], all = TRUE)
          },
          x = dt_list
        )
        ls_mappings[[v]][[d]][[s]] <- merged_dt
      }
    }
  }

  return(ls_mappings)
}
