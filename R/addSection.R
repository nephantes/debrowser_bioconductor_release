#' getAddPanel, conditional panel for additional plots 
#'
#'
#' @note \code{getAddSection}
#' @return the panel for additional plots;
#'
#' @examples
#'     x <- getAddPanel()
#'
#' @export
#'
#'

getAddPanel <- function() {
    a <- list( conditionalPanel( (condition <- "input.addplot=='all2all' ||
                                input.addplot=='heatmap' ||
                                input.addplot=='pca'"),
        conditionalPanel( (condition <- "input.addplot=='heatmap'"),
            column(3, selectInput("clustering_method", "Clustering Method:",
                choices <- c("complete", "ward", "single", "average",
                    "mcquitty", "median", "centroid"))),
            column(3, selectInput("distance_method", "Distance Method:",
                choices <- c("cor", "euclidean", "maximum", "manhattan",
                    "canberra", "binary", "minkowski")))),
        conditionalPanel( (condition <- "input.addplot=='all2all'"),
            column(3, sliderInput("cex", "corr font size",
                                min = 0.1, max = 10,
                                step = 0.1, value = 2))),
            column(3,
                sliderInput("width", "width",
                        min = 100, max = 2000, step = 10, value = 700)),
            column(3,
                sliderInput("height", "height",
                        min = 100, max = 2000, step = 10, value = 500)),
            column(1, actionButton("startAddPlot", "Submit")),
            column(1, downloadButton("downloadPlot", "")),
            uiOutput("plotarea")))
}

#'Left menu for additional plots
#'
#' @note \code{getLeftMenu}
#' @return returns the left menu according to the selected tab;
#'
#' @examples
#'     x <- getLeftMenu()
#'
#' @export
#'
getLeftMenu <- function() {
    a <- list( conditionalPanel( (condition <- "input.methodtabs=='panel1'"),
            wellPanel(radioButtons("mainplot", paste("Main Plots:", sep = ""),
            c(Scatter = "scatter", VolcanoPlot = "volcano",
                MAPlot = "maplot")))),
        conditionalPanel( (condition <- "input.methodtabs=='panel2'"),
            wellPanel(radioButtons("addplot",
                paste("Additional Plots:", sep = ""),
            c(All2All = "all2all", Heatmap = "heatmap", PCA = "pca")))),
        conditionalPanel( (condition <- "input.methodtabs=='panel3'"),
            wellPanel(radioButtons("goplot", paste("Go Plots:", sep = ""),
            c(enrichGO = "enrichGO", enrichKEGG = "enrichKEGG",
        Disease = "disease", compareClusters = "compare")))),
        tags$small("Note: Please don't forget to choose appropriate
            dataset to visualize it in the additional plots."))
}

#' getAddPlots, for additional plots 
#'
#'
#' @note \code{getAddPlots}
#' @return the panel for additional plots;
#' @param dataset, the dataset to use
#' @param datasetname, name of the dataset
#' @param addplot, type of plot to add
#' @param metadata, coupled samples and conditions
#' @param clustering_method, clustering method used
#' @param distance_method, distance method used
#' @param cex, font size
#' @examples
#'     x <- getAddPlots(mtcars)
#'
#' @export
#'
getAddPlots <- function(dataset, datasetname="Up", addplot="heatmap",
                        metadata=NULL,
                        clustering_method = "complete",
                        distance_method = "cor", cex=2) {
    if (nrow(dataset) > 0) {
        if (addplot == "all2all") {
            a <- all2all(dataset, cex)
        } else if (addplot == "heatmap") {
            a <- runHeatmap(dataset, title = paste("Dataset:", datasetname),
                clustering_method = clustering_method,
                distance_method = distance_method)
        } else if (addplot == "pca") {
            colnames(metadata) <- c("samples", "conditions")
            pca_data <- run_pca(getNormalizedMatrix(dataset))
            a <- plot_pca(pca_data$PCs, explained = pca_data$explained,
                metadata = metadata, color = "samples",
                size = 5, shape = "conditions",
                factors = c("samples", "conditions"))
        }
    }
    a
}
