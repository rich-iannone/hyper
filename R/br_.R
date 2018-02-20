#' Create a linebreak
#'
#' Allows for the creation of a linebreak.
#' @inherit p_ return params
#' @importFrom dplyr bind_rows
#' @rdname br_
#' @export
br_ <- function(...,
                id = NULL,
                class = NULL,
                global = NULL) {

  # Define main attributes of constructor
  tag_name <- get_tag_name()
  mode <- "open_close"

  # Gather list of input data
  x_in <- list(...)

  # Generate `id` statement
  id_statement <- generate_id_stmt(id)

  # Generate `class` statement
  class_statement <- generate_class_stmt(class)

  # Generate statements based on `global` atttributes
  global_statements <- get_attr_components(global)

  # Collect all opening tag attributes
  tag_attrs <-
    collect_all_attrs(
      id_statement,
      class_statement,
      global_statements)

  # Create the opening tag
  opening_tag <-
    create_opening_tag(
      type = tag_name,
      attrs_str = tag_attrs)

  # Generate the complete HTML element
  html_element <-
    create_html_element(
      opening_tag = opening_tag,
      closing_tag = NULL,
      content = NULL)

  # Get the input components to the function
  input_component_list <-
    get_input_component_list(input_list = x_in)

  # Case where there is no input object
  if (input_component_list$input_object_count == 0) {

    # Emit the html element
    x_out <- html_element

    return(x_out)
  }

  # Case where there is an input object
  if (input_component_list$input_object_count == 1 &
      input_component_list$input_contains_obj_x) {

    # Collect the existing input HTML object
    input_component_x <- get_object_in_input_x(input_list = x_in)

    # Generate the new input HTML object to be added
    input_component_y <-
      initialize_object(
        type = tag_name,
        mode = mode,
        text = html_element)

    # Combine the HTML objects
    x_out <-
      list(
        stmts =
          dplyr::bind_rows(
            input_component_x$stmts,
            input_component_y$stmts))

    return(x_out)
  }
}
