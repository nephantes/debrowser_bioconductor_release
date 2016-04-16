#' getQCPanel, conditional panel for QC plots 
#' @param flag, to show the section
#'
#' @note \code{getQCSection}
#' @return the panel for QC plots;
#'
#' @examples
#'     x <- getQCPanel()
#'
#' @export
#'

getQCPanel <- function(flag = FALSE) {
    a <- NULL
    if (flag){
        a <- list(
            conditionalPanel(condition = "!input.startQCPlot & 
            input.qcplot=='heatmap'",
            helpText( "Please select parameters and press the 
            submit button in the left menu
            for the plots" )),
            uiOutput("plotarea"))
    }
    a
}

#' getQCPlots, for quality checks 
#'
#' @note \code{getQCPlots}
#' @param dataset, the dataset to use
#' @param input, user input
#' @param metadata, coupled samples and conditions
#' @param clustering_method, clustering method used
#' @param distance_method, distance method used
#' @param cex, font size
#' @return the panel for QC plots;
#' @examples
#'     x <- getQCPlots()
#'
#' @export
#'
getQCPlots <- function(dataset = NULL, input = NULL,
    metadata = NULL, clustering_method = "complete",
    distance_method = "cor", cex = 2) {
    if (is.null(dataset)) return(NULL)
    a <- NULL
    if (nrow(dataset) > 0) {
        if (input$qcplot == "all2all") {
            a <- all2all(dataset, cex)
        } else if (input$qcplot == "heatmap") {
            a <- runHeatmap(dataset, title = paste("Dataset:", input$dataset),
                clustering_method = clustering_method,
                distance_method = distance_method)
        } else if (input$qcplot == "pca") {
            if (!is.null(metadata)){
                colnames(metadata) <- c("samples", "conditions")
            }
            pca_data <- run_pca(getNormalizedMatrix(dataset))
            a <- plot_pca(pca_data$PCs, input$pcselx, input$pcsely,
                explained = pca_data$explained,
                metadata = metadata, color = "samples",
                size = 5, shape = "conditions",
                factors = c("samples", "conditions"))
        }
    }
    a
}
#' getQCPlotArea
#' 
#' @param input, user input
#' @param flag, flag to show the element in the ui
#' @examples
#'     x <- getQCPlotArea()
#'
#' @export
#'
getQCPlotArea <- function(input = NULL,flag = FALSE)
{
    a <- NULL
    if (flag)
        a <- list(column(12, plotOutput("qcplotout",
            height = input$height, width = input$width)),
            conditionalPanel(condition = "input.qcplot == 'pca'",
            column(12, plotOutput("pcaexplained",
            height = input$height, width = input$width))
            ))
    a
}

#' saveQCPlot, save to pdf
#'
#' @note \code{saveQCPlot}
#' @param filename, filename
#' @param input, input params
#' @param datasetInput, dataset
#' @param cols, selected columns
#' @param conds, selected conditions
#' @param inputQCPlot, clustering method and distance method
#' @examples
#'     saveQCPlot()
#'
#' @export
#'
saveQCPlot <- function(filename = NULL, input = NULL, datasetInput = NULL, 
    cols = NULL, conds = NULL, inputQCPlot = NULL){
    if (is.null(datasetInput)) return(NULL)
    pdf(filename, height = input$height * 0.010370,
        width = input$width * 0.010370)
    if (!is.null(cols)){
        dataset <- datasetInput[, cols]
        metadata <- cbind(cols, conds)
    }else{
        dataset <- datasetInput[,c(input$samples)]
        metadata <- cbind(colnames(dataset), "Conds")
    }
    if (nrow(dataset)>2){
        print(getQCPlots(dataset, input, metadata,
            clustering_method = inputQCPlot$clustering_method,
            distance_method = inputQCPlot$distance_method,
            cex = input$cex))
        print(getPCAexplained(datasetInput, cols, input))
    }
    dev.off()
}
