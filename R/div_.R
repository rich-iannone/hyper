#' Create a div element
#'
#' Allows for the creation of a [div] element.
#' @importFrom dplyr select distinct pull bind_rows
#' @export
div_ <- function(...) {

  # Main attributes of constructor
  type <- "div"
  mode_1 <- "opening"
  mode_2 <- "closing"

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

  # Case where there is an input object
  # and possibly some input components
  if (input_component_list$input_object_count == 1 &
      input_component_list$input_contains_obj_x) {

    x_text_1 <- "<div>"
    x_text_2 <- "</div>"

    input_component_x <- x_in[[1]]

    tag_begin <-
      initialize_object(
        type = type,
        mode = "opening",
        text = x_text_1)

    tag_end <-
      initialize_object(
        type = type,
        mode = "closing",
        text = x_text_2)

    x_out <-
      list(
        stmts =
          dplyr::bind_rows(
            input_component_x$stmts,
            tag_begin$stmts,
            tag_end$stmts))

    return(x_out)
  }

  # Case where there are 2 input objects
  # and possibly some input components
  if (input_component_list$input_object_count == 2 &
      input_component_list$input_contains_obj_x &
      input_component_list$input_contains_obj_y) {

    x_text_1 <- "<div>"
    x_text_2 <- "</div>"

    input_component_x <- x_in[[1]]

    input_component_y <- x_in[[2]]

    tag_begin <-
      initialize_object(
        type = type,
        mode = "opening",
        text = x_text_1)

    tag_end <-
      initialize_object(
        type = type,
        mode = "closing",
        text = x_text_2)

    x_out <-
      list(
        stmts =
          dplyr::bind_rows(
            input_component_x$stmts,
            tag_begin$stmts,
            input_component_y$stmts,
            tag_end$stmts))

    return(x_out)
  }
}
