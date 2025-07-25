#' @importFrom purrr pluck 
#'
#' @keywords internal
#' @noRd
.cleave_math <- function(text, pos) {
  pattern <- "^((?:[^() ]|\\([^)]*\\))*) (.*)$"
  return_comp <- regmatches(x = text,
                            m = regexec(pattern = pattern,
                                        text = text))

  # if (grepl(":", return_comp)) {
  #   browser()
  #   chars <- strsplit(text, "")[[1]]
  #   paren_count <- 0
  #   for (i in 1:length(chars)) {
  #     if (chars[i] == "(") {
  #       paren_count <- paren_count + 1
  #     } else if (chars[i] == ")") {
  #       paren_count <- paren_count - 1
  #     }
  #     
  #     # When we're at balanced parentheses and find a space, this is our split point
  #     if (paren_count == 0 && chars[i] == " " && i > 1 && chars[i-1] == ")") {
  #       left_part <- substr(text, 1, i-1)
  #       right_part <- substr(text, i+1, nchar(text))
  #       return(list(conditions = left_part, equation = right_part))
  #     }
  #   }
  # }
  # 

  return_comp <- purrr::pluck(.x = return_comp, 1, pos)
  
  # here we need an algo to identify the no set entries
  if (length(return_comp) != 0) {
    if (identical(x = pos, 2L)) {
      if (!grepl(pattern = "\\(", x = return_comp)) {
        return_comp <- NA
      }
    }
  } else {
    return_comp <- NA
  }
  
  return(return_comp)
}
