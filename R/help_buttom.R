#' @name help_buttom
#' @title Add a question mark helper button
#' @description Shortcut to add an action button with a question mark help icon. USE INTERNAL ONLY.
#' @importFrom shinyhelper helper
#' @export
#' @keywords internal
#' @note Use internal only.
#' @param local A shiny input element.
#' @param body Text to include in the body.
#' @param title Text to include in the title.




help_buttom <- function(local, body, title = "Ajuda"){

  helper(shiny_tag = local,
         type = "inline",
         title = title,
         content = body,
         buttonLabel = "Fechar",
         fade = TRUE,
         size = "m")

}
