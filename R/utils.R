
# Initialize an HTML object with whichever
# `type`, `mode`, or `text` is required
#' @importFrom dplyr tibble
initialize_object <- function(x,
                              type,
                              mode,
                              text) {

  list(
    stmts = dplyr::tibble(
      type = type,
      mode = mode,
      text = text))
}


# Add a statement to the HTML object (`x`);
# this is to be used internally by each
# HTML constructor function whenever the
# HTML object is passed to it
#' @importFrom dplyr tibble bind_rows
add_statement <- function(x,
                          type,
                          mode,
                          text) {

  stmt <-
    dplyr::tibble(
      type = as.character(type),
      mode = as.character(mode),
      text = as.character(text))

  x$stmts <-
    dplyr::bind_rows(
      x$stmts,
      stmt)

  x
}


# Create a table based on an input list; this
# is to detect whether there is the main data
# object somewhere in the list
#' @importFrom purrr map_df
#' @importFrom dplyr tibble
#' @importFrom rlang has_name
get_input_component_tbl <- function(input_list) {

  seq(input_list) %>%
    purrr::map_df(
      .f = function(x) {
        dplyr::tibble(
          list_item = x,
          is_object = ifelse(
            inherits(input_list[x], "list") &
              input_list[[x]] %>% rlang::has_name(name = "stmts"),
            TRUE, FALSE))})
}


# Get a count of input components in the
# table returned by `get_input_components()`
count_input_components <- function(component_tbl) {
  nrow(component_tbl)
}


# Determine whether the input components contains
# an HTML object as the first item
#' @importFrom dplyr pull
is_object_in_input_x <- function(component_tbl) {

  if (nrow(component_tbl) < 1) return(FALSE)

  (component_tbl %>%
     dplyr::pull(is_object))[1]
}


# Determine whether the input components contain
# an HTML object as the second item
#' @importFrom dplyr pull
is_object_in_input_y <- function(component_tbl) {

  if (nrow(component_tbl) < 2) return(FALSE)

  (component_tbl %>%
     dplyr::pull(is_object))[2]
}


# Get a count of all input components
#' @importFrom dplyr filter
count_input_objects <- function(component_tbl) {

  if (nrow(component_tbl) < 1) return(0)

  component_tbl %>%
    dplyr::filter(is_object) %>%
    nrow()
}


# Get a list with information on the
# input components
get_input_component_list <- function(object_list) {

  input_component_tbl <- get_input_component_tbl(object_list)
  input_component_count <- count_input_components(input_component_tbl)
  input_contains_obj_x <- is_object_in_input_x(input_component_tbl)
  input_contains_obj_y <- is_object_in_input_y(input_component_tbl)
  input_object_count <- count_input_objects(input_component_tbl)

  list(
    input_component_tbl = input_component_tbl,
    input_component_count = input_component_count,
    input_contains_obj_x = input_contains_obj_x,
    input_contains_obj_y = input_contains_obj_y,
    input_object_count = input_object_count)
}


# Wrap text in tags and optionally strip away
# existing tags
wrap_in_tags <- function(text, tag, strip = NULL) {

  if (!is.null(strip)) {

    text <-
      text %>%
      stringr::str_replace_all(
        pattern = glue::glue("(<{strip}>|</{strip}>)"),
        replacement = "")
  }

  glue::glue("<{tag}>{text}</{tag}>") %>%
    as.character()
}


# Construct HTML using the HTML object
#' @importFrom dplyr select pull
generate_html_lines <- function(x) {

  x$stmts %>%
    dplyr::select(text) %>%
    dplyr::pull(text) %>%
    paste(collapse = "\n")
}

