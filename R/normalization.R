#' getNormalized matrix
#' it requires edgeR package
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
    M[, colnames(M)] <- apply(M[, colnames(M)], 2,
        function(x) as.integer(x))
    norm.factors <- calcNormFactors(M, method = method)
    return(equalizeLibSizes(DGEList(M,
        norm.factors = norm.factors))$pseudo.counts)
}
