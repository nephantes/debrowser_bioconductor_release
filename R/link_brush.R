#' link_brush
#'
#' Modified linked brush object.
#' A link brush function modified to be able to create non-reactive
#' linked brush object for ggvis plots
#'
#' @note \code{link_brush} is very new and is likely to change
#'     substantially
#' @return A list with components:
#'     \item{input}{A function that takes a visualisation as an argument and
#'      adds an input brush to that plot}
#'     \item{selected}{A reactive providing a logical vector that describes
#'     which points are under the brush}
#'
#' @export
#' @examples
#'     lb <- link_brush()
#'
link_brush <- function() {
    rv <- shiny::reactiveValues(under_brush = character())
    input <- function(vis) {
        handle_brush(vis, fill = "red", on_move = function(items, ...) {
            rv$under_brush <- unique(items$key__)
        })
    }
    selected_r <- reactive(rv$under_brush)
    list(input = input, selected = create_broker(selected_r))
}
