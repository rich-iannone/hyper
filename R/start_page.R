#' Create a page object
#'
#' Create an HTML page object, which can
#' be passed to other builder functions.
#' This starts a page with the
#' necessary components.
#' @examples
#' # Start building an HTML page with the
#' # `start_page()` function
#' page <- start_page()
#'
#' # The page is a simple list object
#' page
#'
#' # The page can be viewed in RStudio
#' # with the `show_in_viewer()` function,
#' # but, let's add some viewable content
#' # first with `p_()`:
#' page %>%
#'   p_("**Hello!** This is just text.") %>%
#'   show_in_viewer()
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
