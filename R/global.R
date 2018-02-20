#' Insert global attributes for the tag
#' @description This helper function should be
#' invoked to provide values for the namesake
#' \code{global} argument, which is present
#' in any function that generates an HTML tag.
#' @param style an inline CSS style for the element.
#' @param title used for extra information about the element.
#' @param lang the language for the element's content.
#' @param hidden when \code{TRUE}, this states that the
#' element is not yet, or perhaps no longer, relevant.
#' @param data this stores custom data private to the page
#' or animation.
#' @param accesskey specifies a shortcut key to
#' activate/focus an element.
#' @param contenteditable can either be \code{TRUE} or
#' \code{FALSE} and this specifies whether the content of
#' an element is editable or not.
#' @param contextmenu specifies a context menu for an element.
#' The context menu appears when a user right-clicks on the
#' element. The value of the contextmenu attribute is the id
#' of the \code{<menu>} element to open.
#' @param dir specifies the text direction of the element's
#' content. Possible values are \code{ltr} (left to right)
#' \code{rtl} (right to left), or \code{auto} (which leaves
#' the browser to figure out the text direction).
#' @param draggable specifies whether an element is
#' draggable or not. This can either be \code{TRUE},
#' \code{FALSE}, or \code{auto} (which uses the default
#' behavior of the browser).
#' @param dropzone specifies whether the dragged data is
#' copied, moved, or linked, when it is dropped on an
#' element. This attribute can have either of these values:
#' \code{copy} (dropped data will result in a copy of the
#' dragged data), \code{move} (dropped data will result
#' in the movement of the dragged data to the new location),
#' and \code{link} (dropped data will result in a link to the
#' original data).
#' @param spellcheck specifies whether the element is to
#' have its spelling and grammar checked or not. The
#' following can be spellchecked: (1) text values in
#' input elements (not passwords though), (2) text in
#' \code{<textarea>} elements, and (3) text in editable
#' elements. This can either be \code{TRUE} or \code{FALSE}.
#' @param translate specifies whether the content of an
#' element should be translated or not. This can either be
#' \code{yes} or \code{no}.
#' @importFrom purrr map_chr
#' @export

global <- function(style = NULL,
                   title = NULL,
                   lang = NULL,
                   hidden = NULL,
                   data = NULL,
                   accesskey = NULL,
                   contenteditable = NULL,
                   contextmenu = NULL,
                   dir = NULL,
                   draggable = NULL,
                   dropzone = NULL,
                   spellcheck = NULL,
                   translate = NULL) {

  # Collect vectors of global attribute
  # values into a list object
  global_attr_values <-
    list(
      style = style,
      title = title,
      lang = lang,
      hidden = hidden,
      data = data,
      accesskey = accesskey,
      contenteditable = contenteditable,
      contextmenu = contextmenu,
      dir = dir,
      draggable = draggable,
      dropzone = dropzone,
      spellcheck = spellcheck,
      translate = translate)

  non_null_attrs <-
    1:length(global_attr_values) %>%
    purrr::map_chr(.f = function(x) {
      if (!is.null(global_attr_values[[x]])) {
        global_attr_values[x] %>% names()
      } else {
        NA
      }
    })

  non_null_attrs <- non_null_attrs[which(!is.na(non_null_attrs))]

  global_attr_values[non_null_attrs]
}
