#' Modified linked brush object.
#'
#' A linked brush function modified to be able to create non-reactive
#' linked brush object for ggvis plots
#'
#' @note \code{delinked_brush} is very new and is likely to change
#'     substantially
#' @return A list with components:
#'     \item{input}{A function that takes a visualisation as an argument and
#'      adds an input brush to that plot}
#'     \item{selected}{A reactive providing a logical vector that describes
#'     which points are under the brush}
#'
#' @export
#' @examples
#'     lb <- linked_brush_scatter()
#'

linked_brush_scatter <- function() {

    rv <- shiny::reactiveValues(under_brush = character())

    input <- function(vis) {
        handle_brush(vis, fill = "red", on_move = function(items, ...) {
            rv$under_brush <- unique(items$key__)
        })
    }

    selected_r <- reactive(rv$under_brush)
    list(input = input, selected = create_broker(selected_r))
}
#' Modified linked brush object.
#'
#' A linked brush function modified to be able to create non-reactive
#' linked brush object for ggvis plots
#'
#' @note \code{delinked_brush} is very new and is likely to change
#'     substantially
#' @return A list with components:
#'     \item{input}{A function that takes a visualisation as an argument and
#'     adds an input brush to that plot}
#'     \item{selected}{A reactive providing a logical vector that describes
#'     which points are under the brush}
#'
#' @examples
#'     lb <- linked_brush_volcano()
#' @export

linked_brush_volcano <- function() {
    rv1 <- shiny::reactiveValues(under_brush1 = character())

    input <- function(vis) {
        handle_brush(vis, fill = "red", on_move = function(items, ...) {
            rv1$under_brush1 <- unique(items$key__)
        })
    }

    selected_r1 <- reactive(rv1$under_brush1)

    list(input = input, selected = create_broker(selected_r1))
}

#' Modified linked brush object.
#'
#' A linked brush function modified to be able to create non-reactive
#' linked brush object for ggvis plots
#'
#' @note \code{delinked_brush} is very new and is likely to change
#'     substantially
#' @return A list with components:
#'     \item{input}{A function that takes a visualisation as an argument and
#'     adds an input brush to that plot}
#'     \item{selected}{A reactive providing a logical vector that describes
#'     which points are under the brush}
#' @examples
#'     lb <- linked_brush_ma()
#' @export
linked_brush_ma <- function() {
    rv2 <- shiny::reactiveValues(under_brush2 = character())

    input <- function(vis) {
        handle_brush(vis, fill = "red", on_move = function(items, ...) {
            rv2$under_brush2 <- unique(items$key__)
        })
    }

    selected_r2 <- reactive(rv2$under_brush2)
    list(input = input, selected = create_broker(selected_r2))
}
