.set_ele_read <- function(file,
                          col,
                          set_name) {
  r_file <- read.csv(file = file)[,2]
  r_file <- sort(x = unique(x = r_file))
  ls_ele <- list(r_file)
  names(x = ls_ele) <- set_name
  return(ls_ele)
}
