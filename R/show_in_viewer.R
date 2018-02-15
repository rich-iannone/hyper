#' Show constructed HTML content in the
#' RStudio Viewer pane
#'
#' View the state of the HTML object
#' in the RStudio Viewer. Stepwise
#' constructor functions are used to
#' build the page.
#' @importFrom dplyr pull
#' @importFrom rstudioapi viewer
#' @export
show_in_viewer <- function(x) {

  html_lines <- generate_html_lines(x)

  temp_dir <- tempfile()

  dir.create(temp_dir)

  html_file <- file.path(temp_dir, "test.html")

  writeLines(html_lines, html_file)

  rstudioapi::viewer(html_file)
}
