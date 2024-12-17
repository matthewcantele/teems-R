#' @keywords internal
#' @noRd
.set_message <- function(set_ele,
                         set_name,
                         model_set) {

  # Loop through each element in set_ele
  element_text <- paste(set_ele[[1]], collapse = "\n")
  message_text <- paste("Set elements for:",
                        paste0(set_name, "s"),
                        paste0("(", model_set, ")"),
                        "determined as:\n")
  message_text <- paste0(message_text, element_text, "\n")

  # Print the final message
  message(message_text)
}
