#' @importFrom data.table fread is.data.table as.data.table
#' 
#' @keywords internal
#' @noRd
.shock_preload <- function(input,
                           type) {
  UseMethod(".shock_preload")
}

#' @method .shock_preload character
#' @export
.shock_preload.character <- function(input,
                                     type) {

  input <- .check_input(
    file = input,
    valid_ext = "csv",
    call = call
  )
  input <- data.table::fread(input)
  .chk_shk_col(input = input,
               type = type,
               call = call)
  return(input)
}

#' @method .shock_preload data.frame
#' @export
.shock_preload.data.frame <- function(input,
                                      type) {

  if (!data.table::is.data.table(input)) {
    input <- data.table::as.data.table(input)
  }
  
  .chk_shk_col(input = input,
               type = type,
               call = call)
  
  return(input)
}