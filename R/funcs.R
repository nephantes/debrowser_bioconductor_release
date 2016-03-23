#' Common functions
#'
#' push an object to the list
#'
#' @param l, that are going to push to the list
#' @param ..., list object
#' @return combined list
#'
#' @export
#'
#' @examples
#'     mylist <- list()
#'     newlist <- push ( 1, mylist )

push <- function(l, ...) c(l, list(...))

#' Plot PCA results.
#' @param l, the value
#' @return round value
#' @export
#'
#' @examples
#'     x<-round_vals(5.1323223)

round_vals <- function(l) {
    l <- round(as.numeric(l), digits = 2)
    parse(text = l)
}
