

# Create a table based on an input list; this
# is to detect whether there is the main data
# object somewhere in the list
#' @importFrom purrr map_df
#' @importFrom dplyr tibble
#' @importFrom rlang has_name
supplied_components_tbl <- function(object_list) {

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

