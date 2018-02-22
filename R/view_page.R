#' View the HTML page
#'
#' View the HTML object in the RStudio Viewer.
#' @importFrom dplyr pull
#' @importFrom rstudioapi viewer
#' @export
view_page <- function(x) {

  html_lines <- generate_html_lines(x)

  temp_dir <- tempfile()

  dir.create(temp_dir)

  html_file <- file.path(temp_dir, "test.html")

  writeLines(html_lines, html_file)

  rstudioapi::viewer(html_file)
}
