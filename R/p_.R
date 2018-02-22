#' Create a paragraph element
#'
#' Allows for the creation of an HTML paragraph
#' (\code{<p>...</p>}) with whichever content is
#' nested within.
#' @param ... a collection of HTML objects, textual
#' elements (as bare text or Markdown-styled text),
#' arbitrary attributes (given as named variables).
#' @param id a global attribute for specifying a
#' unique ID for an HTML element (the value must
#' be unique within the HTML document, though, this
#' is not currently enforced here). The primary use
#' of the \code{id} attribute is to point to a style
#' in a style sheet. The \code{id} is also used by
#' JavaScript (via the HTML DOM) to manipulate the
#' element with the specific \code{id}.
#'
#' When constructing an \code{id}, there are a few
#' other things to note: (1) it must contain at
#' least one character, (2) it must not contain any
#' space characters, and (3) in HTML, all values are
#' case-insensitive.
#' @param class a global attribute for specifying
#' one or more classnames for an element. The
#' \code{class} attribute is primarily used in
#' pointing to a class in a style sheet. However,
#' this attribute can also be used by a JavaScript
#' (via the HTML DOM) to make changes to HTML
#' elements with a specified class. If providing
#' several classes, one can either use
#' \code{c("[classname1]", "[classname2]", ...)} or
#' a single string with space-separated class names.
#'
#' When constructing class names, there are two
#' things to keep in mind: (1) they begin with a
#' letter (\code{A-Z} or \code{a-z}), and (2)
#' subsequent characters can be letters
#' (\code{A-Za-z}), digits (\code{0-9}), hyphens,
#' and underscores.
#' @param global provides an opportunity to supply
#' global attributes other than the \code{id} and
#' \code{class} attributes (which have their own
#' arguments). This is most easily accomplished via
#' the namesake \code{global()} function since it
#' allows for inline help and validation for each
#' of the global attributes. For example, setting
#' the global attributes \code{title} and
#' \code{lang} can be done by using this:
#' \code{global = global(lang = "en", title = "my_title")}.
#' @importFrom commonmark markdown_html
#' @importFrom dplyr bind_rows
#' @export
p_ <- function(...,
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
        level = 2L,
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
        level = 2L,
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
