# .onLoad <- function(libname, pkgname) {
#   # Define cache directories
#   data_cache <- tools::R_user_dir("teems", "data")
#   config_cache <- tools::R_user_dir("teems", "config")
#   
#   # Function to safely remove directory contents
#   clear_cache_dir <- function(cache_path, dir_type) {
#     if (dir.exists(cache_path)) {
#       tryCatch({
#         # Remove all files and subdirectories
#         unlink(cache_path, recursive = TRUE, force = TRUE)
#         # Optionally recreate the empty directory
#         dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
#         message(sprintf("Cleared %s cache: %s", dir_type, cache_path))
#       }, error = function(e) {
#         warning(sprintf("Failed to clear %s cache at %s: %s", 
#                         dir_type, cache_path, e$message))
#       })
#     } else {
#       message(sprintf("No %s cache found at: %s", dir_type, cache_path))
#     }
#   }
#   
#   # Clear both cache directories
#   clear_cache_dir(data_cache, "data")
#   clear_cache_dir(config_cache, "config")
#   
#   # Optional: Add any other package initialization code here
#   invisible()
# }

# implement option for this directory