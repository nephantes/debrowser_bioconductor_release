#' load_data.
#'
#' Loads user selected data to be used for DESeq
#'
#' @param input, input values
#' @param session, if data is going to be loaded from json
#' @return data
#' @export
#'
#' @examples
#'     x<-load_data ()
load_data <- function (input = NULL, session = NULL) {
    if (is.null(input)) return(NULL)
    loaddemo <- reactiveValues(demo = NULL, demodata = NULL)
    loaddemo <- eventReactive(input$demo, {
        m <- c()
        m$demo <- TRUE
        load(system.file("extdata", "demo", "demodata.Rda",
            package = "debrowser"))
        #load(paste0("./demo/demodata.Rda"))
        m$demodata <- demodata
        m
    })
    query <- parseQueryString(session$clientData$url_search)
    jsonobj<-query$jsonobject
    if (!is.null(jsonobj))
    {
        jsondata<-fromJSON(jsonobj, simplifyDataFrame = TRUE)
        rownames(jsondata)<-jsondata[, 1]
        jsondata<-jsondata[,c(2:ncol(jsondata))]
        return(jsondata)
    }
    if (is.null(input$file1) && is.null(loaddemo()$demo)) {
    # User has not uploaded a file yet
        return(NULL)
    }
    else if (is.null(input$file1) && loaddemo()$demo == TRUE) {
        return(loaddemo()$demodata)
    }
    inFile <- input$file1
    try(m <- read.table(inFile$datapath, sep = "\t",
        header = TRUE, row.names = 1))
    
    m
}
