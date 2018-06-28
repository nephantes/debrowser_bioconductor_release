#' debrowserall2all
#'
#' Module for a bar plot that can be used in data prep, main plots 
#' low count removal modules or any desired module
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @param cex, the size of the dots
#' @return all2all plot 
#' @export
#'
#' @examples
#'     x <- debrowserall2all()
#'
debrowserall2all <- function(input, output, session, data = NULL,
                                 cex=2) {
    if(is.null(data)) return(NULL)
    output$all2allplot <- renderPlot({
        all2all(data, cex)
    })
    output$all2allUI <- renderUI({
        shinydashboard::box(
            collapsible = TRUE, title = "All2all plot", status = "primary", 
            solidHeader = TRUE, width = NULL,
            draggable = TRUE,  plotOutput(session$ns("all2allplot"),
                width = input$width, height=input$height))
    })
}

#' getAll2AllPlotUI
#'
#' all2all plots UI.  
#'
#' @note \code{getAll2AllPlotUI}
#' @param id, namespace id
#' @return the panel for all2all plots;
#'
#' @examples
#'     x <- getAll2AllPlotUI("bar")
#'
#' @export
#'
getAll2AllPlotUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("all2allUI"))
}

#' all2allControlsUI
#'
#' Generates the controls in the left menu for an all2all plot
#'
#' @note \code{all2allControlsUI}
#' @param id, namespace id
#' @return returns the controls for left menu
#' @examples
#'     x <- all2allControlsUI("bar")
#' @export
#'
all2allControlsUI <- function(id) {
    ns <- NS(id)
    shinydashboard::menuItem(paste0(id, " - Options"),
         sliderInput("cex", "corr font size",
                min = 0.1, max = 10,
                step = 0.1, value = 2)
    )
}

#' all2all
#'
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

#' panel.hist
#'
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

#' panel.cor
#'
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
