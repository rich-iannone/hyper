#' Create an opening HTML tag
#'
#' Create an opening HTML tag either in
#' standalone mode or as part of an
#' HTML-build pipeline.
#' @importFrom dplyr pull
#' @importFrom purrr flatten
#' @export
h_html_open <- function(...) {

  # Main attributes of constructor
  type <- "html"
  mode <- "opening"
  text <- "<html>"

  x <- list(...)

  if (length(x) == 0) {
    return(text)
  }

  # Get the input components to the function
  input_components_tbl <- get_input_components(x)

  if ((input_components_tbl %>% dplyr::pull(is_object))[1]) {

    x <- (x %>% purrr::flatten())[1]
    x <- add_statement(x, type, mode, text)
    return(x)

  } else {

    return(text)
  }
}
