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
#' @import edgeR
#' @export
#'
#'
getNormalizedMatrix <- function(M, method = "TMM") {
    norm.factors <- calcNormFactors(M, method = method)
    return(equalizeLibSizes(DGEList(M,
        norm.factors = norm.factors))$pseudo.counts)
}
