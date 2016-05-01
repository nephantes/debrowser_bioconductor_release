#' getSampleNames
#'
#' Prepares initial samples to fill condition boxes.  
#' it reads the sample names from the data and splits into two. 
#'
#' @param cnames, sample names in the header of a dataset
#' @param part, c(1,2). 1=first half and 2= second half
#' @return sample names.
#'
#' @examples
#'     x<-getSampleNames()
#' @export
#'
getSampleNames <- function(cnames = NULL, part = 1) {
    if (is.null(cnames)) return(NULL)
      
    startpos <- 1
    endpos <- length(cnames)
    if (part == 1)
        endpos <- floor(endpos / 2) 
    else if (part == 0)
        startpos <- floor(endpos / 2) + 1

    cn <- cnames[startpos:endpos]
    m <- as.list(NULL)
    for (i in seq(cn)) {
        m[[i]] <- cn[i]
    }
    m
}
