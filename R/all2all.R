#' Prepares all2all scatter plots for given datasets. 
#'
#' @param data, data that have the sample names in the header.
#' @param cex text size
#' @return all2all scatter plots
#' @examples
#'     plot<-all2all(mtcars)
#'
#' @export
#'

all2all <- function(data, cex=2) {
    pcor <- function(x, y, ...) panel.cor(x, y, cex.cor = cex)
    nr <- nrow(data)
    if (nr > 1000)
        nr <- 1000
    pairs(log10(data[1:nr, ]), cex = 0.25,
            diag.panel = panel.hist, lower.panel = pcor)
}

#' Prepares the historgram for the all2all plot. 
#'
#' @param x, a vector of values for which the histogram is desired
#' @param ..., any additional params
#' @return all2all histogram plots
#' @examples
#'     panel.hist(1)
#'
#' @export
#'
panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nb <- length(breaks)
    y <- h$counts
    y <- y / max(y)
    rect(breaks[-nb], 0, breaks[-1], y, col = "red", ...)
}

#' Prepares the correlations for the all2all plot. 
#'
#' @param x, numeric vector x
#' @param y, numeric vector y
#' @param prefix, prefix for the text
#' @param cex.cor, correlation font size
#' @param ..., additional parameters
#' @return all2all correlation plots
#' @examples
#'     panel.cor(c(1,2,3), c(4,5,6))
#'
#' @export
#'
panel.cor <- function(x, y, prefix = "rho=", cex.cor=2, ...){
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor.test(x, y, method = "spearman",
        na.rm = TRUE, exact = FALSE)$estimate
    txt <- round(r, digits = 2)
    txt <- paste0(prefix, txt)
    text(0.5, 0.5, txt, cex = cex.cor)
}
