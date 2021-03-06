#' Create headings
#'
#' Allows for the creation of an HTML headings
#' (e.g., \code{<h1>...</h1>}) with whichever content
#' is nested within.
#' @inherit p_ return params
#' @param level the level of heading to use for
#' the \code{h_()} function. Allowed levels are from
#' \code{1} to \code{6}. This argument is not required
#' and not available when using the related \code{h1_()},
#' \code{h2_()}, \code{h3_()}, \code{h4_()},
#' \code{h5_()}, or \code{h6_()} functions.
#' @importFrom commonmark markdown_html
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @rdname h_
#' @export
h_ <- function(...,
               id = NULL,
               class = NULL,
               global = NULL,
               level = 1) {

  # Define main attributes of constructor
  tag_name <- glue::glue("h{level}") %>% as.character()
  mode <- "open_close"

  # Gather list of input data
  x_in <- list(...)

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

    # Generate a standalone HTML object
    x_out <-
      initialize_object(
        type = tag_name,
        mode = mode,
        text = html_element)

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

#' @export
#' @rdname h_
h1_ <- function(..., id = NULL, class = NULL, global = NULL) {
  h_(..., id = id, class = class, global = global, level = 1)
}

#' @export
#' @rdname h_
h2_ <- function(..., id = NULL, class = NULL, global = NULL) {
  h_(..., id = id, class = class, global = global, level = 2)
}

#' @export
#' @rdname h_
h3_ <- function(..., id = NULL, class = NULL, global = NULL) {
  h_(..., id = id, class = class, global = global, level = 3)
}

#' @export
#' @rdname h_
h4_ <- function(..., id = NULL, class = NULL, global = NULL) {
  h_(..., id = id, class = class, global = global, level = 4)
}

#' @export
#' @rdname h_
h5_ <- function(..., id = NULL, class = NULL, global = NULL) {
  h_(..., id = id, class = class, global = global, level = 5)
}

#' @export
#' @rdname h_
h6_ <- function(..., id = NULL, class = NULL, global = NULL) {
  h_(..., id = id, class = class, global = global, level = 6)
}
