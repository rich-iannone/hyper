#' Create an anchor element
#'
#' Allows for the creation of an anchor element. This
#' is for creating a hyperlink to other web pages,
#' files, locations within the same page, email
#' addresses, or any other URL.
#' @inherit p_ return params
#' @param href the URL of the page to which the link
#' is directed.
#' @param target specifies where to open the linked
#' document. There are 4 options here: (1) \code{_self}
#' (the default) opens the linked document in the same
#' frame as it was clicked, (2) \code{_blank} opens the
#' linked document in a new window or tab, (3)
#' \code{_parent} opens the linked document in the
#' parent frame, and (4) \code{_top} opens the linked
#' document in the full body of the window.
#' @param download specifies that the target will be
#' downloaded when a user clicks on the hyperlink. This
#' attribute is only used if the \code{href} attribute
#' is set. The value of the attribute will be the name
#' of the downloaded file. There are no restrictions on
#' allowed values, and the browser will automatically
#' detect the correct file extension and add it to the
#' file (.img, .pdf, .txt, .html, etc.). If the value is
#' omitted, the original filename is used.
#' @param media this specifies what media/device the
#' linked document is optimized for. This attribute is
#' used to specify that the target URL is designed for
#' special devices, speech, or print media. This
#' attribute can accept several values.
#' @param type specifies the Internet media type
#' (formerly known as MIME type) of the linked document.
#' This attribute is only used if the \code{href}
#' attribute is set.
#' @param rel this attribute specifies the relationship
#' between the current document and the linked document.
#' It is only used if the \code{href} attribute is
#' present.
#' @param hreflang specifies the language of the linked
#' document. This attribute is only used if the
#' \code{href} attribute is set.
#' @importFrom dplyr bind_rows
#' @rdname a_
#' @export
a_ <- function(...,
               id = NULL,
               class = NULL,
               global = NULL,
               href = NULL,
               target = NULL,
               download = NULL,
               media = NULL,
               type = NULL,
               rel = NULL,
               hreflang = NULL) {

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

  # Generate `href` statement
  href_statement <- generate_stmt(attr_name = "href", attr_value = href)

  # Generate `target` statement
  target_statement <- generate_stmt(attr_name = "target", attr_value = target)

  # Generate `download` statement
  download_statement <- generate_stmt(attr_name = "download", attr_value = download)

  # Generate `media` statement
  media_statement <- generate_stmt(attr_name = "media", attr_value = media)

  # Generate `type` statement
  type_statement <- generate_stmt(attr_name = "type", attr_value = type)

  # Generate `rel` statement
  rel_statement <- generate_stmt(attr_name = "rel", attr_value = rel)

  # Generate `hreflang` statement
  hreflang_statement <- generate_stmt(attr_name = "hreflang", attr_value = hreflang)

  # Collect all opening tag attributes
  tag_attrs <-
    collect_all_attrs(
      id_statement,
      class_statement,
      global_statements,
      href_statement, target_statement, download_statement,
      media_statement, type_statement, rel_statement, hreflang_statement)

  # Create the opening tag
  opening_tag <-
    create_opening_tag(
      type = tag_name,
      attrs_str = tag_attrs)

  # Create the closing tag
  closing_tag <-
    create_closing_tag(type = tag_name)

  if (has_text_components(input_list = x_in)) {

    # Generate the content for the tag
    content <-
      get_text_components(x_in) %>%
      paste(collapse = " ") %>%
      commonmark::markdown_html() %>%
      strip_outer_tags("p") %>%
      remove_trailing_linebreaks()

  } else {
    content <- ""
  }

  # Generate the complete HTML element
  html_element <-
    create_html_element(
      opening_tag = opening_tag,
      closing_tag = closing_tag,
      content = content)

  html_element
}
