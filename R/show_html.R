#' Show the HTML content
#'
#' Show the generated HTML content in the
#' R console.
#' @export
show_html <- function(x) {

  html_lines <- generate_html_lines(x)

  message("Formatted HTML content:")

  cat(html_lines)

  invisible(x)
}
