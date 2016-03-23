#' PCA analysis
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
    if ( !is.null(x) ){
        pca <- prcomp(t(x), retx = retx,
                center = center, scale. = scale)
        variances <- pca$sdev ^ 2
        explained <- variances / sum(variances)
        return(list(PCs = pca$x, explained = explained))
    }
}

#' Plot PCA results.
#'
#' @param x, dataframe with data
#' @param pcx, x axis label
#' @param pcy, y axis label
#' @param explained, additional axis data
#' @param metadata, additional data
#' @param color, color for plot
#' @param shape, shape for plot
#' @param size, size of the plot
#' @param factors, factors of the plot
#' @return pca list
#' @examples
#'     load(system.file("extdata", "demo", "demodata.Rda",
#'             package="debrowser"))
#'     pca_data<-run_pca(getNormalizedMatrix(
#'             demodata[rowSums(demodata[,2:7])>10,2:7]))
#'     metadata<-cbind(colnames(demodata[,2:7]), 
#'             c(rep("Cond1",3), rep("Cond2",3)))
#'     colnames(metadata)<-c("samples", "conditions")
#'
#'     a <- plot_pca(pca_data$PCs, explained = pca_data$explained,
#'             metadata = metadata, color = "samples",
#'             size = 5, shape = "conditions",
#'             factors = c("samples", "conditions"))
#'
#' @export
#' @import ggplot2
#'
plot_pca <- function(x = NULL, pcx = 1, pcy = 2, explained = NULL,
    metadata = NULL, color = NULL, shape = NULL,
    size = NULL, factors = NULL) {
        if ( is.null(x) ) return(NULL)

        # Prepare data frame to pass to ggplot
        if (!is.null(metadata)) {
            plot_data <- cbind(x, metadata)
            plot_data <- as.data.frame(plot_data)
            # Convert numeric factors to class 'factor'
            for (f in factors) {
                plot_data[, f] <- as.factor(plot_data[, f])
            }
        } else {
            plot_data <- as.data.frame(x)
        }
        # Prepare axis labels
        if (!is.null(explained)) {
            xaxis <- sprintf("PC%d (%.2f%%)", pcx,
                round(explained[pcx] * 100, 2))
            yaxis <- sprintf("PC%d (%.2f%%)", pcy,
                round(explained[pcy] * 100, 2))
        } else {
            xaxis <- paste0("PC", pcx)
            yaxis <- paste0("PC", pcy)
        }
        # Plot
        p <- ggplot(plot_data, aes_string(x = paste0("PC", pcx),
            y = paste0("PC", pcy))) + geom_point(aes_string(color = color,
            shape = shape), size = size) + labs(x = xaxis, y = yaxis) +
            scale_x_discrete(labels = round_vals) +
            scale_y_discrete(labels = round_vals)
        p
}
