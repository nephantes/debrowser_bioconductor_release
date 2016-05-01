#' push
#'
#' Push an object to the list.
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

#' round_vals
#'
#' Plot PCA results.
#'
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

#' getDomains
#'
#' Get domains for the main plots.
#'
#' @param filt_data, data to get the domains
#' @return domains
#' @export
#'
#' @examples
#'     x<-getDomains()
getDomains <- function(filt_data = NULL){
    if (is.null(filt_data)) return (NULL)
    a <- unique(filt_data$Legend)
    a <- a[a != ""]
    if (length(a) == 1)
        a <- c(a, "NA")
    a
}

#' getColors
#'
#' get colors for the domains 
#'
#' @param domains, domains to be colored
#' @return colors
#' @export
#'
#' @examples
#'     x<-getColors()
#'
getColors <- function(domains = NULL){
    if (is.null(domains)) return (NULL)
    colors <- c()
    for ( dn in seq(1:length(domains)) ){
    if (domains[dn] == "NS" || domains[dn] == "NA")
        colors <- c(colors, "#aaa")
    else if (domains[dn] == "Up")
        colors <- c(colors, "green")
    else if (domains[dn] == "Down")
        colors <- c(colors, "red")
    else if (domains[dn] == "MV")
        colors <- c(colors, "orange")
    else if (domains[dn] == "GS")
        colors <- c(colors, "blue")
    } 
    colors
}
