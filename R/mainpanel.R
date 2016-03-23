#' getMainPanel, main panel for volcano, scatter and maplot.  
#' Barplot and box plots are in this page too
#'
#' @note \code{getMainPanel}
#' @return the panel for main plots;
#'
#' @examples
#'     x <- getMainPanel()
#'
#' @export
#'
getMainPanel <- function() {
    a <- list( conditionalPanel( ( condition <- "input.mainplot=='volcano'" ),
            column( 6, wellPanel( ggvisOutput("vplot1") ) ),
            column( 6, wellPanel( ggvisOutput("vplot2") ) ) ),
        conditionalPanel( ( condition <- "input.mainplot=='maplot'"),
            column( 6, wellPanel( ggvisOutput("vplot3") ) ),
            column( 6, wellPanel( ggvisOutput("vplot4") ) ) ),
        conditionalPanel( ( condition <- "input.mainplot=='scatter'"),
            column( 6, wellPanel( ggvisOutput("vplot5") ) ),
            column( 6, wellPanel( ggvisOutput("vplot6") ) ) ),
        column( 6, wellPanel( ggvisOutput("plot3") ) ),
        column( 6, wellPanel( ggvisOutput("plot4") ) ) )
}

#' getStartUp, startup screen 
#'
#' @note \code{getStartUp}
#' @return startup screen 
#'
#' @examples  
#'     x <- getStartUp()
#'
#' @export
#'
getStartUp <- function(){
    a <- list( column( 12, wellPanel(
        tags$small("Please select a file or
                    load the demo data!") ) ))
}

#' getAfterLoad, after loading the data this text will be shown
#'
#' @note \code{getAfterLoad}
#' @return after load text
#'
#' @examples  
#'     x <- getAfterLoad()
#'
#' @export
#'
getAfterLoad <- function(){
    a <- list( column( 12, wellPanel(
        tags$small("Please choose the appropriate conditions for DESeq analysis
                and press 'Run DESeq' button") ) ))
}
