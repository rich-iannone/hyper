
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
get_input_components <- function(object_list) {

  seq(object_list) %>%
    purrr::map_df(
      .f = function(x) {
        dplyr::tibble(
          list_item = x,
          is_object = ifelse(
            inherits(object_list[x], "list") &
              object_list[[x]] %>% rlang::has_name(name = "stmts"),
            TRUE, FALSE))})
}


# Get a count of input components in the
# table returned by `get_input_components()`
count_input_components <- function(component_tbl) {
  nrow(component_tbl)
}


# Determine whether the input components contains
# the HTML object as the first item
is_object_in_input <- function(object_list) {

  (object_list %>%
     dplyr::pull(is_object))[1]
}


# Determine whether the input components contains
# the HTML object as the first item
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

