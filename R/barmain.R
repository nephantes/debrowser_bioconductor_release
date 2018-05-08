#' debrowserbarmainplot
#'
#' Module for a bar plot that can be used in data prep, main plots 
#' low count removal modules or any desired module
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @param conds, conditions
#' @param cols, columns
#' @param key, the gene or region name
#' @return density plot 
#' @export
#'
#' @examples
#'     x <- debrowserbarmainplot()
#'
debrowserbarmainplot <- function(input, output, session, data = NULL,
                                 conds=NULL, cols = NULL, key=NULL) {
    if(is.null(data)) return(NULL)
    output$BarMain <- renderPlotly({
        getBarMainPlot(data, conds, cols, key)
    })
    output$BarMainUI <- renderUI({
    shinydashboard::box(
        collapsible = TRUE, title = session$ns("plot"), status = "primary", 
        solidHeader = TRUE, width = NULL,
        draggable = TRUE,  plotlyOutput(session$ns("BarMain"),
             width = input$width, height=input$height))
    })
}

#' getBarMainPlotUI
#'
#' main bar plots UI.  
#'
#' @note \code{getBarMainPlotUI}
#' @param id, namespace id
#' @return the panel for Density plots;
#'
#' @examples
#'     x <- getBarMainPlotUI("bar")
#'
#' @export
#'
getBarMainPlotUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("BarMainUI"))
}


#' barMainPlotControlsUI
#'
#' Generates the controls in the left menu for a bar main plot
#'
#' @note \code{barMainPlotControlsUI}
#' @param id, namespace id
#' @return returns the controls for left menu
#' @examples
#'     x <- barMainPlotControlsUI("bar")
#' @export
#'
barMainPlotControlsUI <- function(id) {
  ns <- NS(id)
  shinydashboard::menuItem(paste0(id, " - Options"),
      textInput(ns("genename"), "Gene/Region Name", value = "Foxa3" )
  )
}

#' getBarMainPlot
#'
#' Makes Density plots
#'
#' @param data, count or normalized data
#' @param conds, conds
#' @param cols, cols
#' @param key, key
#' @param title, title
#'
#' @export
#'
#' @examples
#'     getBarMainPlot()
#'
getBarMainPlot <- function(data=NULL, conds=NULL, cols = NULL, key=NULL, title = ""){
  if (is.null(data)) return(NULL)
    vardata <- getVariationData(data, conds, cols, key)
    title <- paste(vardata$genename, " variation")
    p <-plot_ly(vardata, x = ~libs, y = ~count, 
            color=~conds, colors=c("red", "blue"), type = "bar") %>%
        plotly::layout(title = title,
            xaxis = list(title = "Samples", categoryorder = "array", 
            categoryarray = vardata$libs),
            yaxis = list(title = "Read Count"),
            margin = list(pad=10))
    p$elementId <- NULL
    p
}
