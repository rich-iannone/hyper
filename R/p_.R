#' Create a paragraph element
#'
#' Allows for the creation of an
#' HTML paragraph ([<p>...</p>]) with
#' whichever content is nested within.
#' @importFrom commonmark markdown_html
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @importFrom rlang prepend
#' @export
p_ <- function(...) {

  # Define main attributes of constructor
  type <- "p"
  mode <- "open_close"

  # Gather list of input data
  x_in <- list(...)

  attrs_spacer <- ifelse(has_attr_components(x_in), " ", "")

  x_attr <- get_attr_components(x_in) %>% paste(collapse = " ")
  x_text <- get_text_components(x_in) %>% paste(collapse = " ")

  open_tag <-
    glue::glue("<{type}{attrs_spacer}{x_attr}>") %>%
    as.character() %>%
    trimws()

  close_tag <-
    glue::glue("</{type}>") %>%
    as.character()

  # Get the input components to the function
  input_component_list <-
    get_input_component_list(input_list = x_in)

  # Case where there are no input components
  if (input_component_list$input_component_count == 0) {

    x_out <-
      initialize_object(
        type = type,
        mode = mode,
        text = glue::glue("{open_tag}{close_tag}") %>%
          as.character())

    return(x_out)
  }

  # Case where there are no input objects
  # but some input components
  if (input_component_list$input_object_count == 0 &
      input_component_list$input_component_count > 0) {

    # Handle text components
    x_text <-
      x_text %>%
      paste(collapse = " ") %>%
      commonmark::markdown_html() %>%
      strip_outer_tags("p") %>%
      remove_trailing_linebreaks() %>%
      rlang::prepend(open_tag) %>%
      append(close_tag) %>%
      paste(collapse = "")

    x_out <-
      initialize_object(
        type = type,
        mode = mode,
        text = x_text)

    return(x_out)
  }

  # Case where there is an input object
  # and possibly some input components
  if (input_component_list$input_object_count == 1 &
      input_component_list$input_contains_obj_x) {

    x_text <-
      x_text %>%
      paste(collapse = " ") %>%
      commonmark::markdown_html() %>%
      strip_outer_tags("p") %>%
      remove_trailing_linebreaks() %>%
      rlang::prepend(open_tag) %>%
      append(close_tag) %>%
      paste(collapse = "")

    input_component_x <- x_in[[1]]

    input_component_y <-
      initialize_object(
        type = type,
        mode = mode,
        text = x_text)

    x_out <-
      list(
        stmts =
          dplyr::bind_rows(
            input_component_x$stmts,
            input_component_y$stmts))

    return(x_out)
  }
}
