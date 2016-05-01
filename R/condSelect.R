#' getConditionSelector
#'
#' Selects user input conditions to run in DESeq.
#'
#' @param num, panel that is going to be shown
#' @param choices, sample list
#' @param selected, selected smaple list
#' @examples
#'     x <- getConditionSelector()
#'
#' @export
#'
getConditionSelector<- function(num=0, choices = NULL, selected = NULL) {
    if (!is.null(choices))
        a <- list(column(6, selectInput(paste0("condition", num), 
            label = paste0("Condition ", num),
            choices = choices, multiple = TRUE,
            selected = selected)))
}

#' selectConditions
#'
#' Selects user input conditions, multiple if present, to be
#' used in DESeq.
#'
#' @param Dataset, used dataset 
#' @param choicecounter, total number of comparisons
#' @param input, input params
#' @note \code{selectConditions}
#' @return the panel for go plots;
#'
#' @examples
#'     x<- selectConditions()
#'
#' @export
#'
selectConditions<-function(Dataset = NULL,
    choicecounter, input = NULL){
    if (is.null(Dataset)) return(NULL)
    selectedGenes <- function(num){
        if (is.null(input[[paste0("condition", num)]]))
            getSampleNames(input$samples, num %% 2 )
        else
            input[[paste0("condition", num)]]
    }
    nc <- choicecounter$nc
    if (nc >= 0) {
        allsamples <- getSampleNames( input$samples, "all" )
        lapply(seq_len(nc), function(i) {
            selected1 <- selectedGenes(2 * i - 1)
            selected2 <- selectedGenes( 2 * i )
            list(getConditionSelector((2 * i - 1), 
                allsamples, selected1),
            getConditionSelector((2 * i), 
                allsamples, selected2))
    })
    }
}
