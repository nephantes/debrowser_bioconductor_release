#' shinyServer to be able to run interactively
#'
#' @note \code{deServer}
#' @param input, input params from UI
#' @param output, output params to UI
#' @param session, session variable
#' @return the panel for main plots;
#'
#' @examples
#'     deServer
#'
#' @export
#' @import clusterProfiler
#' @importFrom shiny actionButton actionLink addResourcePath column
#'             conditionalPanel downloadButton downloadHandler
#'             eventReactive fileInput fluidPage helpText isolate
#'             mainPanel need observe outputOptions plotOutput
#'             radioButtons reactive reactiveValues renderPlot
#'             renderUI runApp selectInput shinyUI sidebarLayout
#'             sidebarPanel sliderInput tabPanel tabsetPanel
#'             textOutput titlePanel uiOutput updateRadioButtons
#'             updateTabsetPanel wellPanel tags h4 isolate
#'             shinyServer observeEvent updateTextInput HTML
#'             textInput parseQueryString img numericInput
#' @importFrom shinyjs show hide enable disable useShinyjs
#' @importFrom DT datatable dataTableOutput renderDataTable formatStyle
#'             styleInterval
#' @importFrom ggplot2 aes_string geom_point ggplot labs ylab
#'             scale_x_discrete scale_y_discrete aes geom_bar
#'             autoplot
#' @importFrom ggvis add_axis add_legend add_tooltip axis_props
#'             bind_shiny create_broker ggvis ggvisOutput handle_brush
#'             hide_legend layer_bars layer_boxplots layer_points
#'             scale_nominal set_options %>% group_by
#' @importFrom igraph layout.kamada.kawai  
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics barplot hist pairs par rect text
#' @importFrom stats aggregate as.dist cor cor.test dist
#'             hclust kmeans na.omit prcomp var sd
#' @importFrom utils read.table write.table
#' @importFrom DOSE enrichDO enrichMap gseaplot
#' @importMethodsFrom AnnotationDbi as.data.frame as.list colnames
#'             head mappedkeys nrow subset
#' @importMethodsFrom GenomicRanges as.factor
#' @importMethodsFrom IRanges as.matrix "colnames<-" mean
#'             nchar paste rownames toupper unique which
#' @importMethodsFrom S4Vectors t
#' @importMethodsFrom SummarizedExperiment cbind
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_rand_strings
#' @importFrom ReactomePA enrichPathway
#' @importFrom edgeR calcNormFactors equalizeLibSizes DGEList
#' @importFrom DESeq2 DESeq results DESeqDataSetFromMatrix
#' @importFrom org.Hs.eg.db org.Hs.egSYMBOL2EG
#' @importFrom annotate geneSymbols

deServer <- function(input, output, session) {
    tryCatch(
    {
        if (!interactive()) {
            options( shiny.maxRequestSize = 30 * 1024 ^ 2,
                shiny.fullstacktrace = TRUE, shiny.trace=TRUE, 
                shiny.autoreload=TRUE)
        }
        output$programtitle <- renderUI({
            togglePanels(0, c(0), session)
            getProgramTitle(session)
        })
        output$mainpanel <- renderUI({
            a <- NULL
            if (!is.null(randstr()))
                a <- getMainPanel(randstr())
            a
        })
        output$qcpanel <- renderUI({
            getQCPanel(!is.null(init_data()))
        })
        output$plotarea <- renderUI({
            getQCPlotArea(input, !is.null(init_data()))
        })
        output$gopanel <- renderUI({
            getGoPanel(!is.null(init_data()))
        })
        output$cutoffSelection <- renderUI({
            a <- NULL
            nc <- 1
            if (!is.null(choicecounter$nc)) nc <- choicecounter$nc
            if (!is.null(isolate(comparison()$init_data)))
                a <- getCutOffSelection(!is.null(
                isolate(comparison()$init_data)), nc)
            a
        })
        output$downloadSection <- renderUI({
            a <- NULL
            if (!is.null(input$goQCplots) && input$goQCplots)
                getDownloadSection(!is.null(init_data()), "QC")
            else if (!is.null(comparison()$init_data))
                getDownloadSection(!is.null(comparison()$init_data), 
                    "main")
        })
        output$preppanel <- renderUI({
            getDataPrepPanel(!is.null(init_data))
        })
        output$leftMenu  <- renderUI({
           getLeftMenu()
        })
        output$initialmenu <-renderUI({
            getInitialMenu(input, output, session)
        })
        output$loading <- renderUI({
            getLoadingMsg()
        })
        output$logo <- renderUI({
            getLogo()
        })
        output$startup <- renderUI({
            getStartupMsg()
        })
        output$afterload <- renderUI({
            getAfterLoadMsg()
        })
        output$mainmsgs <- renderUI({
            if (is.null(condmsg$text))
                getStartPlotsMsg()
            else
                condmsg$text 
        })
        output$dataready <- reactive({
            return(!is.null(Dataset()))
        })
        outputOptions(output, "dataready", 
            suspendWhenHidden = FALSE)
        Dataset <- reactive({
            load_data(input, session)
        })
        choicecounter <- reactiveValues(nc = 0)
        observeEvent(input$add_btn, {
            shinyjs::enable("goButton")
            choicecounter$nc <- choicecounter$nc + 1}
        )
        observeEvent(input$rm_btn, {
        if (choicecounter$nc > 0) 
            choicecounter$nc <- choicecounter$nc - 1
        if (choicecounter$nc == 0) 
           shinyjs::disable("goButton")
        })
        observeEvent(input$startDESeq, {
            shinyjs::disable("goButton")
            hideObj(c("goQCplots", "startDESeq"))
            showObj(c("add_btn","rm_btn","goButton", "fittype"))
        })
   
        observeEvent(input$resetsamples, {
            updateTextInput(session, "samples",value = "" )
            session$sendCustomMessage("startDESeq", NULL)
            showObj(c("goQCplots", "startDESeq"))
            hideObj(c("add_btn","rm_btn","goButton", "fittype"))
            choicecounter$nc <- 0
        })

        samples <- reactive({
            if (is.null(Dataset())) return(NULL)
                getSamples(colnames(Dataset()), index = 2)
        })

        output$sampleSelector <- renderUI({
            if (is.null(samples())) return(NULL)
            if (is.null(input$samples))
                samp <- samples()
            else
                samp <- input$samples
            a <- list(selectInput("samples",
                label = "Samples",
                choices = samp, multiple = TRUE,
                selected = samp)
            )
        })

        output$conditionSelector <- renderUI({
            selectConditions(Dataset(), choicecounter, input)
        })
        dc <- reactive({
            prepDataContainer(Dataset(), choicecounter$nc, 
            input, session)
        })
        observeEvent(input$goQCplots, {
            togglePanels(2, c(2, 4, 8, 9), session)
        })

        comparison <- reactive({
            compselect <- 1
            if (!is.null(input$compselect))
            compselect <- as.integer(input$compselect)
            dc()[[compselect]]
        })
        conds <- reactive({ comparison()$conds })
        cols <- reactive({ comparison()$cols })
        init_data <- reactive({ 
            if (!is.null(comparison()$init_data)){
                comparison()$init_data 
            }
            else
                qcdata()
        })
        filt_data <- reactive({
            if (!is.null(comparison()$init_data) 
            && !is.null(input$padjtxt) && 
            !is.null(input$foldChangetxt))
            applyFilters(init_data(), isolate(cols()), input)
        })
        randstr <- reactive({ 
            a<-NULL
            if (!is.null(selected$data$randstr))
                a<-selected$data$randstr() 
            a
        })
        selected <- reactiveValues(data = NULL)
        observe({
            if (!is.null(input$padj)){
                if (input$padj %% 2)
                    valpadj = (10 ^ (-1*as.integer(
                    (10-input$padj)/2 )) ) /2
                else
                    valpadj = (10 ^ (-1*(10-input$padj)/2))
                if(input$padj == 0) valpadj = 0
                updateTextInput(session, "padjtxt",
                    value = valpadj ) 
            }
            if (!is.null(input$gopvalue)){
                if (input$gopvalue%%2)
                    gopval = (10 ^ (-1*as.integer(
                    (10-input$gopvalue)/2 )) ) /2
            else
                gopval = (10 ^ (-1*(10-input$gopvalue)/2))
            if(input$gopvalue==0) gopval = 0
            updateTextInput(session, "pvaluetxt",
                value = gopval ) 
            }
            if (!is.null(input$foldChange)){
                valpadjfoldChange = input$foldChange
                updateTextInput(session, "foldChangetxt",
                        value = valpadjfoldChange)
            }
        })
        condmsg <- reactiveValues(text = NULL)
        observeEvent(input$startPlots, {
            compselect <- 1
            if (!is.null(input$compselect) ) 
                compselect <- as.integer(input$compselect)
            if (!is.null(isolate(filt_data())) && !is.null(input$padjtxt) && 
                !is.null(input$foldChangetxt)) {
                condmsg$text <- getCondMsg( isolate(cols()), isolate(conds()))
                selected$data<-getMainPanelPlots(isolate(filt_data()), 
                    isolate(cols()), isolate(conds()), input, compselect)
            }
        })
        qcdata <- reactive({
            prepDataForQC(Dataset()[input$samples])
        })
        output$qcplotout <- renderPlot({
            a <- NULL
            if (!is.null(input$qcplot)) {
                if (!is.null(cols())){
                    dataset <- datasetInput()[, cols()]
                    metadata <- cbind(cols(), conds())
                }else{
                    dataset <- datasetInput()[,c(input$samples)]
                    metadata <- cbind(colnames(dataset), "Conds")
                }
                if (nrow(dataset)>2)
                    a <- getQCPlots(dataset, input, metadata,
                        clustering_method = inputQCPlot()$clustering_method,
                        distance_method = inputQCPlot()$distance_method,
                        cex = input$cex)
            }
            a
        })

        output$pcaexplained <- renderPlot({
            a <- NULL
            if (!is.null(input$qcplot)) {
                a <- getPCAexplained(datasetInput(), 
                                 cols(), input )
            }
            a
        })

        inputQCPlot <- reactiveValues(clustering_method = "ward.D2",
            distance_method = "cor")
        inputQCPlot <- eventReactive(input$startQCPlot, {
            m <- c()
            m$clustering_method <- input$clustering_method
            m$distance_method <- input$distance_method
            return(m)
        })
        inputGOstart <- eventReactive(input$startGO, {
            return(getGOPlots(isolate(datasetInput())[, isolate(cols())],
                input, table = FALSE))
        })
        output$GOPlots1 <- renderPlot({
            inputGOstart()
        })

        output$table <- DT::renderDataTable({
            if (!is.null(init_data()))
                m <- DT::datatable(init_data(), options =
                list(lengthMenu = list(c(10, 25, 50, 100),
                c("10", "25", "50", "100")),
                pageLength = 25, paging = TRUE, searching = TRUE)) 
            if (!input$goQCplots)
                m %>% getTableStyle(input)
            m
        })

        output$up <- DT::renderDataTable({
            if (!is.null(init_data()))
                DT::datatable(filt_data()[filt_data()[, "Legend"] == "Up", ], 
                options = list(lengthMenu = list(c(10, 25, 50, 100),
                c("10", "25", "50", "100")),
                pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                getTableStyle(input)
        })
        output$down <- DT::renderDataTable({
            if (!is.null(init_data()))
                DT::datatable(filt_data()[filt_data()[, "Legend"] == "Down", ], 
                options = list(lengthMenu = list(c(10, 25, 50, 100),
                c("10", "25", "50", "100")),
                pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                getTableStyle(input)
        })
        output$selected <- DT::renderDataTable({
            if (is.null(selected$data)) return(NULL)
                DT::datatable(selected$data$getSelected(), 
                options = list(lengthMenu = list(c(10, 25, 50, 100),
                c("10", "25", "50", "100")),
                pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                getTableStyle(input)
        })
        output$geneset <- DT::renderDataTable({
            if (is.null(getGeneSet())) return(NULL)
                m <- DT::datatable(getGeneSet(), options =
                    list(lengthMenu = list(c(10, 25, 50, 100),
                    c("10", "25", "50", "100")),
                    pageLength = 25, paging = TRUE, searching = TRUE)) 
            if (!input$goQCplots)
                m %>% getTableStyle(input)
            m
        })
        getGeneSet <- reactive({
            a <- NULL
            if (!input$goQCplots)
                a <- filt_data()[filt_data()$Legend=="GS", ]
            else
                a <- getGeneSetData(data.frame(init_data()), 
                    c(input$genesetarea))
            a
        })
        getMostVaried <- reactive({
            a <- NULL
            if (!input$goQCplots)
                a <- filt_data()[filt_data()$Legend=="MV", ]
            else
                a <- getMostVariedList(data.frame(init_data()), 
                c(input$samples), input$topn, input$mincount)
        a
        })
        output$mostvaried <- DT::renderDataTable({
            m <- DT::datatable(getMostVaried(), options =
                list(lengthMenu = list(c(10, 25, 50, 100),
                c("10", "25", "50", "100")),
                pageLength = 25, paging = TRUE, searching = TRUE)) 
            if (!input$goQCplots)
                m %>% getTableStyle(input)
            m
        })
        output$mergedcomp <- DT::renderDataTable({
            if (is.null(dc())) return(NULL)
                merged <- getMergedComparison(dc(), choicecounter$nc )
                fcstr<-colnames(merged)[grepl("foldChange", colnames(merged))]
                pastr<-colnames(merged)[grepl("padj", colnames(merged))]
                DT::datatable(merged, options =
                    list(lengthMenu = list(c(10, 25, 50, 100),
                    c("10", "25", "50", "100")),
                    pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                getTableStyle(input, pastr, fcstr)
        })
        output$gotable <- DT::renderDataTable({
            if (!is.null(datasetInput()) && input$startGO){
                gorestable <- getGOPlots(datasetInput()[, cols()],
                    input, table = TRUE)
                DT::datatable(gorestable,
                    list(lengthMenu = list(c(10, 25, 50, 100),
                    c("10", "25", "50", "100")),
                    pageLength = 25, paging = TRUE, searching = TRUE))
            }
        })

        datasetInput <- function(addIdFlag = FALSE){
            m <- NULL
            if (!input$goQCplots )
                m <- getSelectedDatasetInput(filt_data(), 
                    selected$data$getSelected(), getMostVaried(), getGeneSet(),
                    getMergedComparison(dc(), choicecounter$nc), input)
            else
                m <- getSelectedDatasetInput(init_data(), 
                    getMostVaried = getMostVaried(), getGeneSet = getGeneSet(), 
                    input = input)
            if(addIdFlag)
                m <- addID(m)
            m
        }
        output$downloadData <- downloadHandler(filename = function() {
            paste(input$dataset, "csv", sep = ".")
        }, content = function(file) {
            write.table(datasetInput(TRUE), file, sep = ",", row.names = FALSE)
        })

        output$downloadPlot <- downloadHandler(filename = function() {
            paste(input$qcplot, ".pdf", sep = "")
        }, content = function(file) {
            if (!input$goQCplots)
                saveQCPlot(file, input, datasetInput(), 
                    cols(), conds(), inputQCPlot())
            else
                saveQCPlot(file, input, datasetInput(),
                    inputQCPlot = inputQCPlot())
        })

        output$downloadGOPlot <- downloadHandler(filename = function() {
            paste(input$goplot, ".pdf", sep = "")
        }, content = function(file) {
            pdf(file)
            print( getGOPlots(datasetInput()[, cols()], input, table = FALSE) )
            dev.off()
        })
    },
    err=function(errorCondition) {
        cat("in err handler")
        message(errorCondition)
    },
    warn=function(warningCondition) {
        cat("in warn handler")
        message(warningCondition)
    })
}
