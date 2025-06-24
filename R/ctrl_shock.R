#' @importFrom rlang expr current_env
#' @importFrom targets tar_target_raw tar_cue
#' 
#' @keywords internal
#' @noRd
.shock_control <- function(shock,
                           shock_file,
                           ndigits,
                           write_dir) {

  if (!is.null(x = shock_file)) {
    t_shock_file <- rlang::expr(targets::tar_target_raw(
      name = "shock_file",
      command = quote(expr = !!shock_file),
      format = "file"
    ))
  }

  if (!is.null(x = shock)) {
    # shock files need to be tracked via targets
    t_shocks <- rlang::expr(targets::tar_target_raw(
      name = "raw_shocks",
      command = quote(expr = !!shock),
      cue = targets::tar_cue(mode = "always")
    ))

    t_constructed.shocks <- rlang::expr(targets::tar_target_raw(
      name = "constructed.shocks",
      command = expression(.shock_construct(
        shocks = raw_shocks,
        closure = swapped.out.cls,
        var_extract = tab_comp[["var_extract"]],
        sets = final.set_tib,
        reference_year = metadata[["reference_year"]]
      )),
      cue = targets::tar_cue(mode = "always")
    ))
  } else if (!is.null(x = shock_file)) {
    t_shocks <- rlang::expr(targets::tar_target_raw(
      name = "shocks",
      command = expression(readLines(con = shock_file)),
      cue = targets::tar_cue(mode = "always")
    ))

    t_constructed.shocks <- rlang::expr(targets::tar_target_raw(
      name = "constructed.shocks",
      command = expression({
        return(list(
          shocks = list(user = list(
            shock = shocks,
            type = "user"
          )),
          shock_file = shock_file
        ))
      }),
      cue = targets::tar_cue(mode = "always")
    ))
  } else {
    t_constructed.shocks <- rlang::expr(targets::tar_target_raw(
      name = "constructed.shocks",
      command = expression({
        return(list(
          shocks = NULL,
          shock_file = paste0(
            format(x = Sys.time(), "%H%M%S"),
            ".shf"
          )
        ))
      }),
      cue = targets::tar_cue(mode = "always")
    ))
  }

  # Write shock(s)
  t_write.shocks <- rlang::expr(targets::tar_target_raw(
    name = "write.shocks",
    command = expression(.TEEMS_write(
      input = constructed.shocks[["shocks"]],
      ndigits = !!ndigits,
      file = constructed.shocks[["shock_file"]],
      write_object = "shock",
      write_dir = !!write_dir
    )),
    cue = targets::tar_cue(mode = "always")
  ))
  ##############################################################################
  # gather and check all generated targets
  envir <- rlang::current_env()
  targets <- .gather_targets(
    criteria = "t_",
    envir = envir
  )
  return(targets)
}
