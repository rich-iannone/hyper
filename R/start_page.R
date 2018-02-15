#' Create a page object
#' @description Create an HTML page object,
#' which can be passed to other builder
#' functions. This starts a page with the
#' necessary document type declaration.
#' @importFrom dplyr tibble
#' @export
#'
start_page <- function() {

  list(
    name = "temp_name",
    stmts = dplyr::tibble(
      type = c("_dtd"),
      mode = c("singleton"),
      text = c("<!DOCTYPE html>")))
}
