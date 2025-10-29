.chk_shk_col <- function(input,
                         type,
                         call) {

  value_colnames <- colnames(input)
  if (!"Value" %=% value_colnames[length(value_colnames)]) {
    .cli_action(
      shk_err$cst_scen_val_df,
      action = "abort",
      url = NULL,
      hyperlink = NULL,
      call = call
    )
  }
  
  if (type %=% "scenario" && !"Year" %in% value_colnames) {
    .cli_action(
      shk_err$scen_year_df,
      action = "abort",
      url = NULL,
      hyperlink = NULL,
      call = call
    )
  }
  return(invisible(NULL))
}