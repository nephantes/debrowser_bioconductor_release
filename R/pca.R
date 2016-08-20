#' run_pca
#'
#' Runs PCA on the selected dataset.
#'
#' @param x, dataframe with experiment data
#' @param retx, specifies if the data should be returned
#' @param center, center the PCA (Boolean)
#' @param scale, scale the PCA (Boolean)
#' @return pca list
#' @examples
#'     load(system.file("extdata", "demo", "demodata.Rda", 
#'         package="debrowser"))
#'     pca_data<-run_pca(getNormalizedMatrix(
#'         demodata[rowSums(demodata[,2:7])>10,2:7]))
#'
#' @export
#'
run_pca <- function(x=NULL, retx = TRUE,
                center = TRUE, scale = TRUE) {
    if ( is.null(x) ) return (NULL)
        pca <- prcomp(t(x), retx = retx,
                center = center, scale. = scale)
        variances <- pca$sdev ^ 2
        explained <- variances / sum(variances)
        return(list(PCs = pca$x, explained = explained))
}

#' plot_pca
#'
#' Plots the PCA results for the selected dataset.
#'
#' @param dat, data
#' @param pcx, x axis label
#' @param pcy, y axis label
#' @param metadata, additional data
#' @param color, color for plot
#' @param shape, shape for plot
#' @param size, size of the plot
#' @param factors, factors of the plot
#' @return pca list
#' @examples
#'     load(system.file("extdata", "demo", "demodata.Rda",
#'             package="debrowser"))
#'     metadata<-cbind(colnames(demodata[,2:7]), 
#'             c(rep("Cond1",3), rep("Cond2",3)))
#'     colnames(metadata)<-c("samples", "conditions")
#'
#'     a <- plot_pca(getNormalizedMatrix(
#'             demodata[rowSums(demodata[,2:7])>10,2:7]),
#'             metadata = metadata, color = "samples",
#'             size = 5, shape = "conditions",
#'             factors = c("samples", "conditions"))
#'
#' @export
#'
plot_pca <- function(dat = NULL, pcx = 1, pcy = 2,
    metadata = NULL, color = NULL, shape = NULL,
    size = NULL, factors = NULL) {
        if ( is.null(dat) ) return(NULL)
    
        pca_data <- run_pca(dat)
        x <- pca_data$PCs
        explained <- pca_data$explained
        plot_data <- data.frame(x)
        # Prepare data frame to pass to ggplot
        if (!is.null(metadata)) {
            plot_data <- cbind(plot_data, metadata)
        } 
        xaxis <- paste0("PC", pcx)
        yaxis <- paste0("PC", pcy)
        p_data <- plot_data[,c(xaxis, yaxis, "samples", "conditions")]
        colnames(p_data) <- c("x", "y", "samples", "conditions")
        # Prepare axis labels
        xaxis <- sprintf("PC%d (%.2f%%)", pcx,
            round(explained[pcx] * 100, 2))
        yaxis <- sprintf("PC%d (%.2f%%)", pcy,
            round(explained[pcy] * 100, 2))

        p_data %>% ggvis(x = ~x, y = ~y) %>% 
            layer_points(size := 100, 
                fill = ~samples, shape =~ conditions) %>%
            add_tooltip(getToolTipPCA, "hover") %>%
            add_axis("x", title = xaxis) %>%
            add_axis("y", title = yaxis) %>%
            hide_legend("shape") %>%
            set_options(duration = 0, width = "auto", height = "auto", 
                resizable = TRUE)
}

#' getToolTipPCA
#'
#' Prepares tooltiptext for PCA plot
#'
#' @param dat, data
#' @return tooltip text
#'
#' @export
#'
#' @examples
#' x <- getToolTipPCA()
#'
getToolTipPCA <- function(dat=NULL){
    if (is.null(dat)) return(NULL)
    
    paste0("<b>", dat$samples, "</b>")
}

#' getPCAexplained
#'
#' Creates a more detailed plot using the PCA results from
#' the selected dataset.
#'
#' @param datasetInput, selected data
#' @param input, from user
#' @return explained plot
#' @examples
#'     x <- getPCAexplained()
#'
#' @export
#'
getPCAexplained <- function(datasetInput = NULL, 
    input = NULL) {
    if (is.null(datasetInput)) return(NULL)
    a <- NULL
    if (input$qcplot == "pca"){
        dataset <- datasetInput[,c(input$col_list)]
        pca_data <- run_pca(getNormalizedMatrix(dataset, input$norm_method))
        datexp <- data.frame(cbind(unlist(lapply(
            c(1:length(pca_data$explained)), 
            function(x){paste0("PC", x)})), 
            round(pca_data$explained * 100, 2)))
        colnames(datexp) <- c("PCs", "explained")
        datexp$explained <- as.numeric( as.character(datexp$explained) )
        datexp %>% ggvis(x = ~PCs, y = ~explained) %>% 
            layer_bars() %>%
            set_options(width = "auto", height = "auto", resizable = TRUE ) %>%
            bind_shiny("ggvisQC2")
    }
    a
}

