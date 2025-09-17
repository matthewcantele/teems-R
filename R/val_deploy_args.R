#' @importFrom tibble is_tibble
.validate_deploy_args <- function(a,
                                  sets,
                                  call) {

  checklist <- list(
    data = "list",
    model = "data.frame",
    write_dir = "character",
    shock = c("NULL", "list"),
    closure_file = c("NULL", "character"),
    swap_in = c("NULL", "character", "list"),
    swap_out = c("NULL", "character", "list"),
    shock_file = c("NULL", "character")
  )
  
  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )
  
  if (!a$write_dir %=% tools::R_user_dir("teems", "cache")) {
    a$write_dir <- normalizePath(a$write_dir)
    if (!dir.exists(a$write_dir)) {
      .cli_action(deploy_err$invalid_write_dir,
        action = "abort",
        call = call
      )
    }
  } else {
    unlink(a$write_dir,
      recursive = TRUE
    )

    dir.create(a$write_dir,
      recursive = TRUE
    )
  }

  int_sets <- subset(a$model,
                     qualifier_list == "(intertemporal)",
                     name,
                     1)
  
  if (nrow(int_sets) %=% 0L) {
    int_sets <- NULL
  }
  
  if (!is.null(a$shock)) {
    a$shock <- lapply(
      a$shock,
      .check_shock,
      var_extract = subset(a$model, type %in% "Variable"),
      int_sets = int_sets,
      call = call
    )
  }

  if (!is.null(a$closure_file)) {
    a$closure_file <- .check_input(
      file = a$closure_file,
      valid_ext = "cls",
      call = call
    )
  }

  a$closure <- .check_closure_file(
    closure_file = a$closure_file,
    tab_file = attr(a$model, "tab_file"),
    var_omit = attr(a$model, "var_omit"),
    var_extract = subset(a$model, type %in% "Variable"),
    call = call
  )

  if (!is.null(a$swap_in)) {
    a$swap_in <- lapply(a$swap_in,
      .check_swap,
      var_extract = subset(a$model, type %in% "Variable"),
      sets = sets,
      call = call
    )
  }

  if (!is.null(a$swap_out)) {
    a$swap_out <- lapply(a$swap_out,
      .check_swap,
      var_extract = subset(a$model, type %in% "Variable"),
      sets = sets,
      call = call
    )
  }
  
  if (!is.null(a$shock_file)) {
    if (!is.null(a$shock)) {
      .cli_action(shk_err$shk_file_shocks,
        action = "abort",
        call = call
      )
    }

    a$shock_file <- .check_input(
      file = a$shock_file,
      valid_ext = "shf",
      call = call
    )
  }
  return(a)
}