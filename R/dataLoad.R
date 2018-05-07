#' debrowserdataload
#'
#' Module to load count data and metadata
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowserdataload()
#'
debrowserdataload <- function(input, output, session) {
    
    ldata <- reactiveValues(count=NULL, meta=NULL)

    observe({
        query <- parseQueryString(session$clientData$url_search)
        jsonobj<-query$jsonobject
        # To test json load;
        # It accepts three parameters:
        # 1. jsonobject=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2F%0D%0Aadvanced_demo.tsv%26format%3DJSON
        # 2. meta=meta=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fsimple_meta.tsv%26format%3DJSON
        # 3. title=no
        # The finished product of the link will look like this without metadata:
        # 
        # https://127.0.0.1:3427/debrowser/R/?jsonobject=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2F%0D%0Aadvanced_demo.tsv%26format%3DJSON&title=no
        #        
        #  With metadata
        #
        #http://127.0.0.1:3427/?jsonobject=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fsimple_demo.tsv%26format%3DJSON&meta=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fsimple_meta.tsv%26format%3DJSON
        #
        #
        
        if (!is.null(jsonobj))
        {
            raw <- RCurl::getURL(jsonobj, .opts = list(ssl.verifypeer = FALSE),
                 crlf = TRUE)
            jsondata<-data.frame(fromJSON(raw, simplifyDataFrame = TRUE),
                                 stringsAsFactors = TRUE)
            rownames(jsondata)<-jsondata[, 1]
            jsondata<-jsondata[,c(3:ncol(jsondata))]
            jsondata[,c(1:ncol(jsondata))] <- sapply(
                jsondata[,c(1:ncol(jsondata))], as.numeric)
            metadatatable <- NULL
            jsonmet <-query$meta
            if(!is.null(jsonmet)){
                raw <- RCurl::getURL(jsonmet, .opts = list(ssl.verifypeer = FALSE),
                    crlf = TRUE)
                metadatatable<-data.frame(fromJSON(raw, simplifyDataFrame = TRUE),
                    stringsAsFactors = TRUE)
                
            }
            ldata$meta <- metadatatable
            ldata$count <- jsondata
        }
    })
    observeEvent(input$demo, {
        load(system.file("extdata", "demo", "demodata.Rda",
                         package = "debrowser"))

        ldata$count <- demodata
        ldata$meta <- metadatatable
    })
    
    observeEvent(input$uploadFile, {
        if (is.null(input$countdata)) return (NULL)
        counttable <-as.data.frame(
            try(
                read.delim(input$countdata$datapath, 
                header=T, sep=input$countdataSep, 
            row.names=1 ), T))
        metadatatable <- c()
        if (!is.null(input$metadata$datapath)){
        metadatatable <- as.data.frame(
            try(
                read.delim(input$metadata$datapath, 
                header=T, sep=input$metadataSep), T))
        }
        if (is.null(counttable)) 
            {stop("Please upload the count file!")}
        ldata$count <- counttable
        ldata$meta <- metadatatable
    })
    loadeddata <- reactive({
        ret <- NULL
        if(!is.null(ldata$count)){
            ret <- list(count = ldata$count, meta = ldata$meta)
        }
        return(ret)
    })
    
    observe({
        getSampleDetails(output, "uploadSummary", "sampleDetails", loadeddata())
    })
  list(load=loadeddata)
}

#' dataLoadUI
#' 
#' Creates a panel to upload the data
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- dataLoadUI("load")
#'
#' @export
#'
dataLoadUI<- function (id) {
  ns <- NS(id)
  list(
        fluidRow(
             fileUploadBox(id, "countdata", "Count Data"),
             fileUploadBox(id, "metadata", "Metadata")
        ),
        fluidRow(column(12,
        actionButton(ns("uploadFile"), label = "Upload", styleclass = "primary"), 
        actionButton(ns("demo"),  label = "Load Demo!", styleclass = "primary"),
        actionButton("Filter", label = "Filter", styleclass = "primary"))
        ),
  fluidRow(
    shinydashboard::box(title = "Upload Summary",
        solidHeader = T, status = "info",
        width = 12, 
        fluidRow(
          column(12, 
              tableOutput(ns("uploadSummary"))
          )),
        fluidRow(
          column(12,div(style = 'overflow: scroll', 
              DT::dataTableOutput(ns("sampleDetails")))
          )
        )
    )
  ))
}

#' fileUploadBox
#'
#' File upload module
#' @param id, namespace id
#' @param inputId, input file ID
#' @param label, label
#' @note \code{fileUploadBox}
#' @return radio control
#'
#' @examples
#'    
#'     x <- fileUploadBox("meta", "metadata", "Metadata")
#'
#' @export
#'
fileUploadBox <- function(id = NULL, inputId = NULL, label = NULL) {
  ns <- NS(id)
  
  shinydashboard::box(title = paste0(label, " File"),
                      solidHeader = TRUE, status = "info",
                      width = 6,
                      helpText(paste0("Upload your '", label," File'")),
                      fileInput(inputId=ns(inputId), 
                                label=NULL, 
                                accept=fileTypes()
                      ),
                      sepRadio(id, paste0(inputId, "Sep")))
}

#' sepRadio
#'
#' Radio button for separators
#'
#' @param id, module id
#' @param name, name
#' @note \code{sepRadio}
#' @return radio control
#'
#' @examples
#'    
#'     x <- sepRadio("meta", "metadata")
#'
#' @export
#'
sepRadio <- function(id, name) {
  ns <- NS(id)
  radioButtons(inputId=ns(name), 
               label="Separator",
               choices=c(Comma=',',
                         Semicolon=';',
                         Tab='\t'
               ),
               selected='\t'
  )
}

#' fileTypes
#'
#' Returns fileTypes that are going to be used in creating fileUpload UI
#'
#' @note \code{fileTypes}
#' @return file types
#'
#' @examples
#'     x <- fileTypes()
#'
#' @export
#'
fileTypes <- function() {
  c('text/tab-separated-values',
    'text/csv',
    'text/comma-separated-values',
    'text/tab-separated-values',
    '.txt',
    '.csv',
    '.tsv')
}