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
p_ <- function(...,
               id = NULL,
               class = NULL) {

  # Define main attributes of constructor
  type <- "p"
  mode <- "open_close"

  # Gather list of input data
  x_in <- list(...)

  # Generate `id` statement
  id_statement <- generate_id_stmt(id)

  # Generate `class` statement
  class_statement <- generate_class_stmt(class)

  # Collect all opening tag attrs
  tag_attrs <-
    collect_all_attrs(
      id_statement,
      class_statement,
      get_attr_components(x_in))

  # Create the opening tag
  opening_tag <-
    create_opening_tag(
      type = type,
      attrs_str = tag_attrs)

  # Create the closing tag
  closing_tag <-
    create_closing_tag(type = type)

  # Generate the content for the tag
  content <-
    get_text_components(x_in) %>%
    paste(collapse = " ") %>%
    commonmark::markdown_html() %>%
    strip_outer_tags("p") %>%
    remove_trailing_linebreaks()

  # Generate the complete HTML element
  html_element <-
    create_html_element(
      opening_tag = opening_tag,
      closing_tag = closing_tag,
      content = content)

  # Get the input components to the function
  input_component_list <-
    get_input_component_list(input_list = x_in)

  # Case where there is no input object
  if (input_component_list$input_object_count == 0) {

    x_out <-
      initialize_object(
        type = type,
        mode = mode,
        text = html_element)

    return(x_out)
  }

  # Case where there is an input object
  if (input_component_list$input_object_count == 1 &
      input_component_list$input_contains_obj_x) {

    input_component_x <- x_in[[1]]

    input_component_y <-
      initialize_object(
        type = type,
        mode = mode,
        text = html_element)

    x_out <-
      list(
        stmts =
          dplyr::bind_rows(
            input_component_x$stmts,
            input_component_y$stmts))

    return(x_out)
  }
}
