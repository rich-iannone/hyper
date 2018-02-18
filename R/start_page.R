#' Create a page object
#'
#' Create an HTML page object, which can
#' be passed to other builder functions.
#' This starts a page with the
#' necessary components.
#' @export
start_page <- function() {

  initialize_object(
    type = c(
      "_dtd", "html", "head", "head", "body"),
    mode = c(
      "empty", "open", "open", "close", "open"),
    text = c(
      "<!DOCTYPE HTML>", "<html>", "<head>", "</head>", "<body>"))
}
