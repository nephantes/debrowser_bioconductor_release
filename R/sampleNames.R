#' Prepares initial samples to fill condition boxes.  
#' it reads the sample names from the data and splits into two. 
#'
#' @param dat, data that have the sample names in the header
#' @param part, c(1,2). 1=first half and 2= second half
#' @return sample names.
#'
#' @examples
#'     x<-getSampleNames(mtcars, 1)
#'     x<-getSampleNames(mtcars, 2)
#' @export
#'

getSampleNames <- function(dat, part) {
    if (is.null(dat)) {
        return(NULL)
    }
    cnames <- colnames(dat)
    start <- 1
    n <- length(cnames)
    end <- floor(n / 2) + 1
    if (part == 2) {
        start <- floor(n / 2) + 2
        end <- n
    }
    cn <- cnames[start:end]
    m <- as.list(NULL)
    for (i in seq(cn)) {
        m[[i]] <- cn[i]
    }
    m
}
