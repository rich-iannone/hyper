
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
              object_list[[x]] %>% rlang::has_name(name = "name") &
              object_list[[x]] %>% rlang::has_name(name = "stmts"),
            TRUE, FALSE))})
}

