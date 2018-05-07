#' getBoxMainPlotUI
#'
#' main Box plots UI.  
#'
#' @note \code{getBoxMainPlotUI}
#' @param id, namespace id
#' @return the panel for Density plots;
#'
#' @examples
#'     x <- getBoxMainPlotUI("box")
#'
#' @export
#'
getBoxMainPlotUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("BoxMainUI"))
}

#' debrowserboxmainplot
#'
#' Module for a box plot that can be used in DEanalysis main part and 
#' used heatmaps
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @return density plot 
#' @export
#'
#' @examples
#'     x <- debrowserboxmainplot()
#'
debrowserboxmainplot <- function(input, output, session, data = NULL,
                                  conds=NULL, cols = NULL, key=NULL) {
  if(is.null(data)) return(NULL)
  output$BoxMain <- renderPlotly({
    getBoxMainPlot(data, conds, cols, key)
  })

  output$BoxMainUI <- renderUI({
    shinydashboard::box(
      collapsible = TRUE, title = session$ns("plot"), status = "primary", 
      solidHeader = TRUE, width = NULL,
      draggable = TRUE,  plotlyOutput(session$ns("BoxMain"),
                                      width = input$width, height=input$height))
  })
}

#' BoxMainPlotControlsUI
#'
#' Generates the controls in the left menu for a Box main plot
#'
#' @note \code{BoxMainPlotControlsUI}
#' @param id, namespace id
#' @return returns the controls for left menu
#' @examples
#'     x <- BoxMainPlotControlsUI("box")
#' @export
#'
BoxMainPlotControlsUI <- function(id) {
  ns <- NS(id)
  shinydashboard::menuItem(paste0(id, " - Options"),
                           textInput(ns("breaks"), "Breaks", value = "100" )
  )
}

#' getBoxMainPlot
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
#'     getBoxMainPlot()
#'
getBoxMainPlot <- function(data=NULL, conds=NULL, cols = NULL, key=NULL, title = ""){
  if (is.null(data)) return(NULL)
  vardata <- getVariationData(data, conds, cols, key)
  
  title <- paste(vardata$genename, " variation")

  p <- plot_ly(vardata, x = ~conds, y = ~count, 
               color=~conds, colors=c("Red", "Blue"),
               boxpoints = "all", type = "box") %>%
    plotly::layout(title = title,
                   xaxis = list(title = "Conditions"),
                   yaxis = list(title = "Read Count"))
  p$elementId <- NULL
  p
}
