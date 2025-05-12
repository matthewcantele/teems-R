#' @keywords internal
#' @noRd
.check_statements <- function(tab,
                              ok_state,
                              call) {
  # remove comments
  n_comments <- paste(unlist(x = strsplit(x = tab, "![^!]*!", perl = TRUE)), collapse = "")
  # break into commands
  statements <- unlist(x = strsplit(x = n_comments, split = ";", perl = TRUE))
  
  # remove new lines and carriage returns
  statements <- gsub(pattern = "\r|\n",
                     replacement = " ",
                     x = statements,
                     perl = TRUE)
  statements <- gsub(pattern = "\\s{2,}",
                     replacement = " ",
                     x = statements)
  statements <- trimws(x = statements[statements != " "])
  
  # check whether statements implicit
  state_decl <- unique(x = sapply(X = strsplit(x = statements, split = " ", perl = TRUE), "[[", 1))
  
  if (any(!is.element(el = state_decl, set = ok_state))) {
    # make implicit decl explicit
    for (s in seq_len(length(x = statements))) {
      statement <- strsplit(statements[s], split = " ")[[1]][1]
      if (!grepl(pattern = paste(ok_state, collapse = "|"),
                 x = statement,
                 ignore.case = TRUE)) {
        implicit_stat <- strsplit(statements[s-1], split = " ")[[1]][1]
        statements[s] <- paste(implicit_stat, statements[s])
      }
    }
  }
  
  # second check
  state_decl <- unique(x = sapply(X = strsplit(x = statements, split = " ", perl = TRUE), "[[", 1))
  if (any(!is.element(el = tolower(x = state_decl), set = tolower(x = ok_state)))) {
    unsupported <- state_decl[is.element(el = tolower(x = state_decl), set = tolower(x = ok_state))]
    .cli_action(msg = "Unsupported Tablo declarations detected: {.val {unsupported}}.",
                action = "abort",
                call = call)
  }
  
  return(statements)
}