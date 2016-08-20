#' getNormalizedMatrix
#'
#' Normalizes the matrix passed to be used within various methods
#' within DEBrowser.  Requires edgeR package
#'
#' @note \code{getGoPanel}
#' @param M, numeric matrix
#' @param method, normalization method for edgeR. default is TMM
#' @return normalized matrix
#'
#' @examples
#'     x <- getNormalizedMatrix(mtcars)
#'
#' @export
#'
getNormalizedMatrix <- function(M = NULL, method = "TMM") {
    if (is.null(M) ) return (NULL)
    M[is.na(M)] <- 0
    M <- subset(M, 
         rowSums(M[,1:ncol(M)]) > 0)
    norm.factors <- calcNormFactors(M, method = method)
    return(equalizeLibSizes(DGEList(M,
        norm.factors = norm.factors))$pseudo.counts)
}
