#' Create a page object
#'
#' Create an HTML page object, which can
#' be passed to other builder functions.
#' This starts a page with the
#' necessary document type declaration
#' and an opening \code{<html>} tag.
#' @importFrom dplyr tibble
#' @export
start_page <- function() {

  initialize_object(
    type = c("_dtd", "html"),
    mode = c("singleton", "opening"),
    text = c("<!DOCTYPE HTML>", "<html>"))
}
