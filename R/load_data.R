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
        jsondata<-data.frame(fromJSON(jsonobj, simplifyDataFrame = TRUE),
                             stringsAsFactors = TRUE)
        rownames(jsondata)<-jsondata[, 1]
        jsondata<-jsondata[,c(2:ncol(jsondata))]
        jsondata[,c(2:ncol(jsondata))] <- sapply(
            jsondata[,c(2:ncol(jsondata))], as.numeric)
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

#' Correct Batch Effect
#'
#' Batch effect correction
#' @param idata, data
#' @param input, input values
#' @return data
#' @export
#'
#' @examples
#'     x<-correctBatchEffect ()
correctBatchEffect <- function (idata = NULL, input = NULL) {
    if (is.null(idata)) return(NULL)
    inFile <- input$file2
    try(metadata <- read.table(inFile$datapath, sep = "\t",
         header = TRUE, row.names = 1))
    batch <- metadata[, input$batchselect]
    columns <- rownames(metadata)
    meta <- data.frame(cbind(columns, batch))
    
    data <- data.frame(idata[, columns])
    data[, columns] <- apply(data[, columns], 2, function(x) as.integer(x))
    
    data[, columns] <- apply(data[, columns], 2, function(x) return(x + runif(1, 0, 0.01)))
    
    modcombat = model.matrix(~1, data = meta)
    
    combat_blind = ComBat(dat=data, batch=batch)
    
    a <- cbind(idata[rownames(combat_blind), 2], combat_blind)
    
    a[, columns] <- apply(a[, columns], 2, function(x) ifelse(x<0, 0, x))
    colnames(a[, 1]) <- colnames(idata[, 1])
    a
}

