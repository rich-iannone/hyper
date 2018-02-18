#' Create headings
#'
#' Create H1-6 tags either in
#' standalone mode or as part of an
#' HTML-build pipeline.
#' @importFrom dplyr pull bind_rows
#' @importFrom purrr flatten
#' @importFrom glue glue
#' @rdname h_
#' @export
h_ <- function(..., level = 1) {

  # Define main attributes of constructor
  type <- glue::glue("h{level}") %>% as.character()
  mode <- "open_close"

  # Gather list of input data
  x_in <- list(...)

  # Get the input components to the function
  input_component_list <-
    get_input_component_list(input_list = x_in)

  # Case where there are no input components
  if (input_component_list$input_component_count == 0) {

    x_out <-
      initialize_object(
        type = type,
        mode = mode,
        text = glue::glue("<{type}></{type}>") %>% as.character())

    return(x_out)
  }

  # Case where there are no input objects
  # but some input components
  if (input_component_list$input_object_count == 0 &
      input_component_list$input_component_count > 0) {

    # Handle text components
    x_text <-
      x_in %>%
      paste(collapse = " ") %>%
      commonmark::markdown_html() %>%
      strip_outer_tags("p") %>%
      remove_trailing_linebreaks %>%
      wrap_in_tags(type)

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

    # Handle text components
    if (input_component_list$input_component_count >
        input_component_list$input_object_count) {

      x_text <-
        get_list_items_as_char(x_in, start = 2) %>%
        paste(collapse = " ") %>%
        commonmark::markdown_html() %>%
        strip_outer_tags("p") %>%
        remove_trailing_linebreaks %>%
        wrap_in_tags(type)

    } else {

      x_text <- "" %>% wrap_in_tags(type)
    }

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

#' @export
#' @rdname h_
h1_ <- function(...) h_(..., level = 1)

#' @export
#' @rdname h_
h2_ <- function(...) h_(..., level = 2)

#' @export
#' @rdname h_
h3_ <- function(...) h_(..., level = 3)

#' @export
#' @rdname h_
h4_ <- function(...) h_(..., level = 4)

#' @export
#' @rdname h_
h5_ <- function(...) h_(..., level = 5)

#' @export
#' @rdname h_
h6_ <- function(...) h_(..., level = 6)
