#' Create a page object
#'
#' Create an HTML page object, which can
#' be passed to other builder functions.
#' This starts a page with the
#' necessary document type declaration.
#' @importFrom dplyr tibble
#' @export
start_page <- function() {

  list(
    stmts = dplyr::tibble(
      type = "_dtd",
      mode = "singleton",
      text = "<!DOCTYPE html>"))
}
