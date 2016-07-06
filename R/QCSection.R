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
    a <- list(
        helpText( "Please select the parameters and press the 
        submit button in the left menu
        for the plots" ),
        conditionalPanel(condition = 
            "(!(input.interactive && input.qcplot == 'heatmap'))",
            column(12, plotOutput("qcplotout",
            height = height, width = width))),
        conditionalPanel(condition = "input.qcplot == 'pca'",
            column(12, plotOutput("pcaexplained",
            height = height, width = width))),
        uiOutput("intheatmap")
       )
    return(a)
}

#' getIntHeatmapVis
#'
#' Gathers the conditional panel for interactive heatmap
#'
#' @param randstr, randstr
#' @note \code{getIntHeatmapVis}
#' @return the panel interactive heatmap
#'
#' @examples
#'     x <- getIntHeatmapVis()
#'
#' @export
#'
getIntHeatmapVis <- function(randstr = NULL) {
    if (is.null(randstr)) return(NULL)
    a <- list(
    conditionalPanel(condition = 
        "(input.qcplot == 'heatmap' && input.interactive)",
        column(12, ggvisOutput(paste0("heatmapplot-", randstr)))
    ))
}
#' getQCPlots
#'
#' Gathers the plot data to be displayed within the
#' quality checks panel.
#'
#' @note \code{getQCPlots}
#' @param dataset, the dataset to use
#' @param input, user input
#' @param metadata, coupled samples and conditions
#' @param inputQCPlot, input QC params
#' @return the panel for QC plots
#' @examples
#'     x <- getQCPlots()
#'
#' @export
#'
getQCPlots <- function(dataset = NULL, input = NULL,
    metadata = NULL, inputQCPlot = NULL) {
    if (is.null(dataset)) return(NULL)
    a <- NULL
    if (nrow(dataset) > 0) {
        if (input$qcplot == "all2all") {
            a <- all2all(dataset, input$cex)
        } else if (input$qcplot == "heatmap") {
            a <- runHeatmap(dataset, title = paste("Dataset:", input$dataset),
                clustering_method = inputQCPlot$clustering_method,
                distance_method = inputQCPlot$distance_method)
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

#' getQCReplot
#'
#' Prepares QCplots for comparisons and others
#' @note \code{getQCReplot}
#' @param cols, the dataset to use
#' @param conds, the dataset to use
#' @param datasetInput, the dataset to use
#' @param input, user input
#' @param inputQCPlot, input QC params
#' @return the panel for QC plots
#' @examples
#'     x <- getQCReplot()
#'
#' @export
#'
getQCReplot <- function(cols = NULL, conds = NULL, 
    datasetInput = NULL, input = NULL, inputQCPlot = NULL){
    if (is.null(datasetInput)) return(NULL)
    if (!is.null(cols) && !input$dataset == "comparisons"){
        dataset <- datasetInput[, cols]
        metadata <- cbind(cols, conds)
    }else{
        dataset <- datasetInput[,c(input$samples)]
        metadata <- cbind(colnames(dataset), "Conds")
    }
    if (nrow(dataset)<3) return(NULL)
    a <- getQCPlots(dataset, input, metadata,
                    inputQCPlot = inputQCPlot)
}
#' saveQCPlot
#'
#' Saves the current QC plot selection to the users local disk.
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
            inputQCPlot = inputQCPlot))
        print(getPCAexplained(datasetInput, cols, input))
    }
    dev.off()
}
