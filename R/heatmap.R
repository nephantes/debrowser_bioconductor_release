#' It creates a heatmap 
#'
#' @param data, a matrixthat includes expression values
#' @param title, title of the heatmap
#' @param dend, dendogram
#' @param names, a flag to show the rownames
#' @param clustering_method = c('complete', 'ward.D2', 'single', 'average',
#' 'mcquitty', 'median' , 'centroid')
#' @param distance_method = c('cor','euclidean', 'maximum', 'manhattan',
#' 'canberra', 'binary' ,'minkowski')
#' @return heatmap.2 plot
#'
#' @examples
#'     x <- runHeatmap(mtcars)
#'
#' @export
#' @import gplots
#' @import RColorBrewer
#'
runHeatmap <- function(data, title="Title", dend = "both",
    names = FALSE,
    clustering_method = c("ward.D2", "complete", "single",
        "average", "mcquitty", "median", "centroid"),
    distance_method = c("euclidean", "cor", "maximum",
        "manhattan", "canberra", "binary", "minkowski")) {
    ld <- log2(data + 0.1)
    cldt <- scale(t(ld), center = TRUE, scale = TRUE)
    cld <- t(cldt)
    hclust2 <- function(x, ...) hclust(x, method = clustering_method)
    dist2 <- function(x, ...) {
        if (distance_method != "cor") {
            return(dist(x, method = distance_method))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }
    m <- heatmap.2(cld, Rowv = TRUE, main = title, dendrogram = dend,
        Colv = TRUE, col = redblue(256), labRow = names,
        distfun = dist2, hclustfun = hclust2, density.info = "none",
        trace = "none", margins = c(10, 10))
    m
}
