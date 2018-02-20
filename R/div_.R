#' Create a div element
#'
#' Allows for the creation of a \code{div} element.
#' @inherit p_ return params
#' @importFrom dplyr select distinct pull bind_rows
#' @export
div_ <- function(...,
                 id = NULL,
                 class = NULL,
                 global = NULL) {

  # Define main attributes of constructor
  tag_name <- get_tag_name()

  mode_1 <- "open"
  mode_2 <- "close"

  # Gather list of input data
  x_in <- list(...)

  # Get the input components to the function
  input_component_list <-
    get_input_component_list(input_list = x_in)

  # If there is no input object, stop function
  if (input_component_list$input_contains_obj_x == FALSE) {
    stop("The `div` element must be contained within <body>...</body>",
         call. = FALSE)
  }

  # Determine whether there is an input component
  # that contains the `_dtd`, `html`, and `body` types
  input_component_lineage <-
    x_in[[1]]$stmts %>%
    dplyr::select(type) %>%
    dplyr::distinct() %>%
    dplyr::pull(type)

  if (!(all(c("_dtd", "html", "body") %in% input_component_lineage))) {

    stop("The `div` element must be contained within <body>...</body>",
         call. = FALSE)
  }

  # Generate `id` statement
  id_statement <- generate_id_stmt(id)

  # Generate `class` statement
  class_statement <- generate_class_stmt(class)

  # Generate statements based on `global` atttributes
  global_statements <- get_attr_components(global)

  # Generate arbitrary statements found in the input list
  extra_statements <- get_attr_components(x_in)

  # Collect all opening tag attributes
  tag_attrs <-
    collect_all_attrs(
      id_statement,
      class_statement,
      global_statements,
      extra_statements)

  # Create the opening tag
  opening_tag <-
    create_opening_tag(
      type = tag_name,
      attrs_str = tag_attrs)

  # Create the closing tag
  closing_tag <-
    create_closing_tag(type = tag_name)

  # Case where there is an input object
  if (input_component_list$input_object_count == 1 &
      input_component_list$input_contains_obj_x) {

    # Collect the existing input HTML object
    input_component_x <- get_object_in_input_x(input_list = x_in)

    tag_begin_section <-
      initialize_object(
        type = tag_name,
        mode = "open",
        text = opening_tag)

    tag_end_section <-
      initialize_object(
        type = tag_name,
        mode = "close",
        text = closing_tag)

    x_out <-
      list(
        stmts =
          dplyr::bind_rows(
            input_component_x$stmts,
            tag_begin_section$stmts,
            tag_end_section$stmts))

    return(x_out)
  }

  # Case where there are 2 input objects
  # and possibly some input components
  if (input_component_list$input_object_count == 2 &
      input_component_list$input_contains_obj_x &
      input_component_list$input_contains_obj_y) {

    # Collect the existing input HTML object
    input_component_x <- get_object_in_input_x(input_list = x_in)

    # Collect the secondary input HTML object
    input_component_y <- get_object_in_input_y(input_list = x_in)

    tag_begin_section <-
      initialize_object(
        type = tag_name,
        mode = "open",
        text = opening_tag)

    tag_end_section <-
      initialize_object(
        type = tag_name,
        mode = "close",
        text = closing_tag)

    x_out <-
      list(
        stmts =
          dplyr::bind_rows(
            input_component_x$stmts,
            tag_begin_section$stmts,
            input_component_y$stmts,
            tag_end_section$stmts))

    return(x_out)
  }
}
