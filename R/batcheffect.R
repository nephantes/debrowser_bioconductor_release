#' debrowserbatcheffect
#'
#' Module to correct batch effect
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param ldata, loaded data
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowserbatcheffect()
#'
debrowserbatcheffect <- function(input, output, session, ldata = NULL) {
    if(is.null(ldata)) return(NULL)
    batchdata <- reactiveValues(count=NULL, meta = NULL)
    observeEvent(input$submitBatchEffect, {
    if (is.null(ldata$count)) return (NULL)

    countData <- ldata$count
    withProgress(message = 'Normalization', detail = "Normalization", value = NULL, {
        if (input$norm_method != "none"){
            countData <- getNormalizedMatrix(ldata$count, method=input$norm_method)
        }
    })
    
    withProgress(message = 'Batch Effect Correction', detail = "Adjusting the Data", value = NULL, {
    if (input$batchmethod == "Combat"){
        batchdata$count <- correctCombat(input, countData, ldata$meta)
    }
    else if (input$batchmethod == "Harman"){
        batchdata$count <- correctHarman(input, countData, ldata$meta)
    }
    else{
        batchdata$count <-  countData
    }
    })
    batchdata$meta <- ldata$meta
  })
  
  output$batchfields <- renderUI({
    if (!is.null(ldata$meta))
        list( conditionalPanel(condition <- paste0("input['", session$ns("batchmethod"),"']!='none'"),
             selectGroupInfo( ldata$meta, input, session$ns("treatment"), "Treatment"),
             selectGroupInfo( ldata$meta, input, session$ns("batch"), "Batch")))
  })
  
  batcheffectdata <- reactive({
    ret <- NULL
    if(!is.null(batchdata$count)){
      ret <- batchdata
    }
    return(ret)
  })
  
  observe({
    getSampleDetails(output, "uploadSummary", "sampleDetails", ldata)
    getSampleDetails(output, "filteredSummary", "filteredDetails", batcheffectdata())
    getTableDetails(output, session, "beforebatchtable", ldata$count, modal=TRUE)
    callModule(debrowserpcaplot, "beforeCorrectionPCA", ldata$count, ldata$meta)
    callModule(debrowserIQRplot, "beforeCorrectionIQR",  ldata$count)
    callModule(debrowserdensityplot, "beforeCorrectionDensity", ldata$count)
    if ( !is.null(batcheffectdata()$count ) && nrow(batcheffectdata()$count)>2 ){
      withProgress(message = 'Drawing the plot', detail = "Preparing!", value = NULL, {
       getTableDetails(output, session, "afterbatchtable", batcheffectdata()$count, modal=TRUE)
       callModule(debrowserpcaplot, "afterCorrectionPCA",  batcheffectdata()$count, batcheffectdata()$meta)
       callModule(debrowserIQRplot, "afterCorrectionIQR",  batcheffectdata()$count)
       callModule(debrowserdensityplot, "afterCorrectionDensity", batcheffectdata()$count)
      })
    }
  })
  
  list(BatchEffect=batcheffectdata)
}


#' batchEffectUI
#' Creates a panel to coorect batch effect
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- batchEffectUI("batcheffect")
#'
#' @export
#'
batchEffectUI <- function (id) {
  ns <- NS(id)
  list(
    fluidRow(
        shinydashboard::box(title = "Batch Effect Correction and Normalization",
        solidHeader = T, status = "info",  width = 12, 
        fluidRow(
            column(5,div(style = 'overflow: scroll',
                tableOutput(ns("uploadSummary")),
                DT::dataTableOutput(ns("sampleDetails"))),
                uiOutput(ns("beforebatchtable"))
            ),
            column(2,
            shinydashboard::box(title = "Options",
                solidHeader = T, status = "info",
                width = 12, 
                normalizationMethods(id),
                batchMethod(id),
                uiOutput(ns("batchfields")),
                actionButton(ns("submitBatchEffect"), label = "Submit", styleclass = "primary")
           )
          ),
          column(5,div(style = 'overflow: scroll', 
                tableOutput(ns("filteredSummary")),
                DT::dataTableOutput(ns("filteredDetails"))),
                uiOutput(ns("afterbatchtable"))
          )
        )),
      shinydashboard::box(title = "Plots",
        solidHeader = T, status = "info",  width = 12, 
        fluidRow(column(1, div()),
            tabsetPanel( id = ns("batchTabs"),
                tabPanel(id = ns("PCA"), "PCA",
                    column(5,
                        getPCAPlotUI(ns("beforeCorrectionPCA"))),
                    column(2,  shinydashboard::box(title = "Before Correction",
                        solidHeader = TRUE, status = "info",
                        width = 12,
                        pcaPlotControlsUI(ns("beforeCorrectionPCA"))),
                        shinydashboard::box(title = "After Correction",
                        solidHeader = TRUE, status = "info",
                        width = 12,
                        pcaPlotControlsUI(ns("afterCorrectionPCA")))),
                    column(5,
                        getPCAPlotUI(ns("afterCorrectionPCA")))
                ),
                tabPanel(id = ns("IQR"), "IQR",
                    column(5,
                        getIQRPlotUI(ns("beforeCorrectionIQR"))),
                    column(2, div()),
                    column(5,
                        getIQRPlotUI(ns("afterCorrectionIQR")))
                ),
                tabPanel(id = ns("Density"), "Density",
                    column(5,
                        getDensityPlotUI(ns("beforeCorrectionDensity"))),
                    column(2, div()),
                    column(5,
                        getDensityPlotUI(ns("afterCorrectionDensity")))
                )
            )
        )
      )
    ))
}
#' normalizationMethods
#'
#' Select box to select normalization method prior to batch effect correction
#'
#' @note \code{normalizationMethods}
#' @param id, namespace id
#' @return radio control
#'
#' @examples
#'    
#'     x <- normalizationMethods()
#'
#' @export
#'
normalizationMethods <- function(id) {
    ns <- NS(id)
    selectInput(ns("norm_method"), "Normalization Method:",
        choices <- c("none", "DESeq2", "TMM", "RLE", "upperquartile"))
}

#' batchMethod
#'
#' select batch effect method
#' @param id, namespace id
#' @note \code{batchMethod}
#' @return radio control
#'
#' @examples
#'    
#'     x <- batchMethod("batch")
#'
#' @export
#'
batchMethod <- function(id) {
  ns <- NS(id)
  selectInput(ns("batchmethod"), "Correction Method:",
              choices <- c("none", "Combat", "Harman"),
               selected='Combat'
  )
}

#' Correct Batch Effect using Combat in sva package
#'
#' Batch effect correction
#' @param input, input values
#' @param idata, data
#' @param metadata, metadata
#' @return data
#' @export
#'
#' @examples
#'     x<-correctCombat ()
correctCombat <- function (input = NULL, idata = NULL, metadata = NULL) {
  if (is.null(idata) || input$batch == "None") return(NULL)
  batch <- metadata[, input$batch]
  treatment <- metadata[, input$treatment]
  columns <- colnames(idata)
  meta <- data.frame(cbind(columns, treatment, batch))
  datacor <- data.frame(idata[, columns])
  datacor[, columns] <- apply(datacor[, columns], 2,
                              function(x) as.integer(x))
  
  datacor[, columns] <- apply(datacor[, columns], 2,
                              function(x) return(x + runif(1, 0, 0.01)))
  
  modcombat = model.matrix(~1, data = meta)
  
  combat_blind = sva::ComBat(dat=as.matrix(datacor), batch=batch)
  
  a <- cbind(idata[rownames(combat_blind), 2], combat_blind)
  
  a[, columns] <- apply(a[, columns], 2, function(x) ifelse(x<0, 0, x))
  colnames(a[, 1]) <- colnames(idata[, 1])
  a[,columns]
}

#' Correct Batch Effect using Harman
#'
#' Batch effect correction
#' @param input, input values
#' @param idata, data
#' @param metadata, metadata
#' @return data
#' @export
#'
#' @examples
#'     x<-correctHarman ()
correctHarman <- function (input = NULL, idata = NULL, metadata = NULL) {
  if (is.null(idata)) return(NULL)
  batch.info <- data.frame(metadata[, c(input$treatment, input$batch)])
  rownames(batch.info) <- rownames(metadata)
  colnames(batch.info) <- c("treatment", "batch") 
  
  harman.res <- harman(idata, expt= batch.info$treatment, batch= batch.info$batch, limit=0.95)
  harman.corrected <- reconstructData(harman.res)
  harman.corrected
}