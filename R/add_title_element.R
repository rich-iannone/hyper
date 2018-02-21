#' Add the title element to the HTML head
#'
#' Add a title to the HTML document.
#' @param title the title of the HTML document.
#' @importFrom dplyr select distinct pull filter
#' @importFrom tibble rownames_to_column add_row
#' @export
add_title_element <- function(x,
                              title) {

  # Gather list of input data
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

  if (!(all(c("_dtd", "html", "head", "body") %in% input_component_lineage))) {

    stop("A title element can only be added once the main elements of the page are available",
         call. = FALSE)
  }

  x_text <- wrap_in_tags(text = title, tag = "title")

  head_end_row <-
    x$stmts %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(type == "head" & mode == "close") %>%
    dplyr::pull(rowname) %>%
    as.numeric()

  x$stmts <-
    x$stmts %>%
    tibble::add_row(
      type = "title",
      mode = "open_close",
      text = x_text,
      .before = head_end_row)

  x
}
