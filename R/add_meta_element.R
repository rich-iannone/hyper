#' Add a meta element to the HTML head
#'
#' Add metadata to the HTML document with a
#' \code{<meta>} element.
#' @importFrom dplyr select distinct pull filter
#' @importFrom tibble rownames_to_column add_row
#' @export
add_meta_element <- function(x,
                             name = NULL,
                             content = NULL,
                             charset = NULL,
                             http_equiv = NULL) {

  x_in <- list(x)

  # Get the input components to the function
  input_component_list <-
    get_input_component_list(input_list = x_in)

  # If there is no input object, stop function
  if (input_component_list$input_contains_obj_x == FALSE) {
    stop("There is no input element",
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

    stop("A meta element can only be added once the main elements of the page are available",
         call. = FALSE)
  }

  if (!is.null(name) & is.null(content)) {
    stop("The `content` is required if `name` is supplied",
         call. = FALSE)
  }

  if (!is.null(http_equiv) & is.null(content)) {
    stop("The `content` is required if `http_equiv` is supplied",
         call. = FALSE)
  }

  if (!is.null(content) & (is.null(name) & is.null(http_equiv))) {
    stop("If `content` is supplied then either `name` or `http_equiv` are required",
         call. = FALSE)
  }

  if (!is.null(name) & !is.null(http_equiv)) {
    stop("Values cannot be supplied for both `name` and for `http_equiv`",
         call. = FALSE)
  }

  # Define attributes for `<meta charset=...>`-type element
  if (!is.null(charset)) {
    tag_attrs <-
      generate_stmt(
        attr_name = "charset",
        attr_value = charset)
  }

  # Define attributes for `<meta name=... content=...>`-type element
  if (!is.null(name) & !is.null(content)) {
    tag_attrs <-
      paste(
        generate_stmt(
          attr_name = "name",
          attr_value = name),
        generate_stmt(
          attr_name = "content",
          attr_value = content),
        collapse = " ")
  }

  # Define attributes for `<meta http-equiv=... content=...>`-type element
  if (!is.null(name) & !is.null(content)) {
    tag_attrs <-
      paste(
        generate_stmt(
          attr_name = "http-equiv",
          attr_value = http_equiv),
        generate_stmt(
          attr_name = "content",
          attr_value = content),
        collapse = " ")
  }

  # Create the opening tag
  opening_tag <-
    create_opening_tag(
      type = "meta",
      attrs_str = tag_attrs)


  head_end_row <-
    x$stmts %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(type == "head" & mode == "close") %>%
    dplyr::pull(rowname) %>%
    as.numeric()

  x$stmts <-
    x$stmts %>%
    tibble::add_row(
      type = "meta",
      mode = "open_close",
      level = 2L,
      text = opening_tag,
      .before = head_end_row)

  x
}
