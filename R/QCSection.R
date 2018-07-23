#' getQCPanel
#'
#' Gathers the conditional panel for QC plots
#'
#' @param input, user input
#' @note \code{getQCSection}
#' @return the panel for QC plots
#'
#' @examples
#'     x <- getQCPanel()
#'
#' @export
#'
getQCPanel <- function(input = NULL) {
    height = "700"
    width = "500"
    if (!is.null(input)) {
        height = input$height
        width = input$width
    }
    qcPanel <- list(
        wellPanel(helpText( "Please select the parameters and press the 
        submit button in the left menu for the plots" ),
        getHelpButton("method", 
        "http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#quality-control-plots")),
        conditionalPanel(condition = "input.qcplot == 'IQR' 
            || input.qcplot == 'Density'",
            column(12, ggvisOutput("ggvisQC1")),
            column(12, ggvisOutput("ggvisQC2"))
        ),
        conditionalPanel(condition = "input.qcplot == 'pca'",
            getPCAPlotUI("qcpca")),    
        conditionalPanel(condition = "(input.qcplot == 'heatmap')",
            getHeatmapUI("heatmapQC")),
        conditionalPanel(condition = "(input.qcplot == 'IQR')",
            getIQRPlotUI("IQR"),
            getIQRPlotUI("normIQR")),
        conditionalPanel(condition = "(input.qcplot == 'Density')",
            getDensityPlotUI("density"),
            getDensityPlotUI("normdensity")),
        conditionalPanel(condition = "(input.qcplot == 'all2all')",
            getAll2AllPlotUI("all2all"))
       )
    return(qcPanel)
}

#' getSelectedCols
#'
#' gets selected columns
#'
#' @param data, all loaded data
#' @param datasetInput, selected dataset
#' @param input, user input params 
#'
#' @export
#'
#' @examples
#'     getSelectedCols()
#'
#'
getSelectedCols <- function(data = NULL, datasetInput = NULL, input=NULL){
    if(is.null(data) || is.null(datasetInput)) return(NULL)
    selCols <- NULL
    if (!is.null(input$dataset)){
        all <- input$samples
        selection <- input$col_list
        if("All" %in% input$col_list || length(input$col_list) == 0){
            selection <- all
        }else{
            selection <- input$col_list
        }
        if (!is.null(selection))
            selCols <- data[rownames(datasetInput), selection]
    }
    return(selCols)
}