
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
get_input_component_list <- function(input_list) {

  input_component_tbl <- get_input_component_tbl(input_list)
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


# Strip away tags of the provided name
#' @importFrom stringr str_replace_all
#' @importFrom glue glue
strip_outer_tags <- function(text, to_strip) {

  text %>%
    stringr::str_replace_all(
      pattern = glue::glue("(<{to_strip}>|</{to_strip}>)"),
      replacement = "")
}


# Remove trailing linebreaks from a character vector
#' @importFrom stringr str_replace
remove_trailing_linebreaks <- function(text) {

  text %>%
    stringr::str_replace(
      pattern = "\n$",
      replacement = "")
}


# Are any components of the input list element attributes?
#' @importFrom rlang have_name
has_attr_components <- function(input_list) {

  any(rlang::have_name(input_list[seq(length(input_list))]))
}


# Are any components of the input list entirely textual?
#' @importFrom rlang have_name
has_text_components <- function(input_list) {

  are_text_components <- vector(mode = "logical")

  for (i in seq(length(input_list))) {

    are_text_components <-
      c(are_text_components,
        !rlang::have_name(input_list[i]) &
          inherits(input_list[[i]], "character"))
  }

  any(are_text_components)
}


# Extract attribute components from an input list
#' @importFrom rlang is_named
#' @importFrom glue glue
get_attr_components <- function(input_list) {

  html_attrs <- vector(mode = "character")

  for (i in seq(length(input_list))) {

    if (rlang::is_named(input_list[i])) {
      html_attrs <-
        c(html_attrs,
          glue::glue("{names(input_list[i])}=\"{unname(input_list[i])}\"") %>%
            as.character())
    }
  }

  html_attrs
}


# Extract textual components from an input list
#' @importFrom rlang have_name
get_text_components <- function(input_list) {

  are_text_components <- vector(mode = "logical")

  for (i in seq(length(input_list))) {

    are_text_components <-
      c(are_text_components,
        !rlang::have_name(input_list[i]) &
          inherits(input_list[[i]], "character"))
  }

  input_list[which(are_text_components)] %>%
    as.character()
}

# Generate an `id` statement
generate_id_stmt <- function(id) {

  if (!is.null(id)) {
    id_statement <- paste0("id=\"", id[1], "\"")
  } else {
    id_statement <- ""
  }

  id_statement
}


# Generate a `class` statement
generate_class_stmt <- function(class) {

  if (!is.null(class)) {
    class_statement <- paste0("class=\"", paste(class, collapse = " "), "\"")
  } else {
    class_statement <- ""
  }

  class_statement
}


# Collect all attr statements (whichever they may
# be for a given tag) and generate a clean string
# to insert in the opening tag
#' @importFrom rlang squash_chr
collect_all_attrs <- function(...) {

  list(...) %>%
    rlang::squash_chr() %>%
    paste(collapse = " ") %>%
    trimws()
}


# Create an opening tag from a predefined type
# and an attribute string
#' @importFrom glue glue
create_opening_tag <- function(type, attrs_str) {

  if (attrs_str != "" & attrs_str != " ") {
    glue::glue("<{type} {attrs_str}>") %>%
      as.character() %>% trimws()
  } else {
    glue::glue("<{type}>") %>% as.character()
  }
}


# Create a closing tag from a predefined type
#' @importFrom glue glue
create_closing_tag <- function(type) {

  glue::glue("</{type}>") %>% as.character()
}


# Generate a complete HTML element with opening and
# closing tags, and, the content
#' @importFrom glue glue
create_html_element <- function(opening_tag,
                                closing_tag,
                                content) {

  glue::glue("{opening_tag}{content}{closing_tag}") %>%
    as.character()
}


# Wrap text in tags and optionally strip away
# existing tags
#' @importFrom glue glue
wrap_in_tags <- function(text, tag, strip = NULL) {

  if (!is.null(strip)) {

    text <-
      text %>%
      strip_outer_tags(to_strip = strip)
  }

  glue::glue("<{tag}>{text}</{tag}>") %>%
    as.character()
}


# With an input list, extract components with
# index greater than the `start` value and
# create a character vector from those
get_list_items_as_char <- function(input_list, start = 2) {

  input_list[start:length(input_list)] %>%
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

