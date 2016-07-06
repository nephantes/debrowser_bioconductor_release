#' deServer
#'
#' Sets up shinyServer to be able to run DEBrowser interactively.
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
#' @import     clusterProfiler
#' @importFrom shiny  actionButton  actionLink  addResourcePath  column 
#'             conditionalPanel  downloadButton  downloadHandler 
#'             eventReactive  fileInput  fluidPage  helpText  isolate 
#'             mainPanel  need  numericInput  observe  observeEvent 
#'             outputOptions  parseQueryString  plotOutput  radioButtons 
#'             reactive  reactiveValues  renderPlot  renderUI  runApp 
#'             selectInput  shinyApp  shinyServer  shinyUI  sidebarLayout 
#'             sidebarPanel  sliderInput  stopApp  tabPanel  tabsetPanel 
#'             textInput  textOutput  titlePanel  uiOutput tags HTML
#'             h4 img icon updateTabsetPanel  updateTextInput  validate 
#'             wellPanel checkboxInput
#' @importFrom shinyjs show hide enable disable useShinyjs extendShinyjs
#'             js inlineCSS
#' @importFrom DT datatable dataTableOutput renderDataTable formatStyle
#'             styleInterval
#' @importFrom ggplot2 aes aes_string geom_bar geom_point ggplot
#'             labs scale_x_discrete scale_y_discrete ylab
#'             autoplot
#' @importFrom ggvis add_axis add_legend add_tooltip axis_props
#'             bind_shiny create_broker ggvis ggvisOutput handle_brush
#'             hide_legend layer_bars layer_boxplots layer_points
#'             scale_nominal set_options %>% group_by layer_rects
#'             band scale_numeric hide_axis
#' @importFrom gplots heatmap.2 redblue
#' @importFrom igraph layout.kamada.kawai  
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics barplot hist pairs par rect text
#' @importFrom stats aggregate as.dist cor cor.test dist
#'             hclust kmeans na.omit prcomp var sd
#' @importFrom utils read.table write.table update.packages
#' @importFrom DOSE enrichDO enrichMap gseaplot
#' @importMethodsFrom AnnotationDbi as.data.frame as.list colnames
#'             head mappedkeys ncol nrow subset keys mapIds
#' @importMethodsFrom GenomicRanges as.factor
#' @importMethodsFrom IRanges as.matrix "colnames<-" mean
#'             nchar paste rownames toupper unique which
#'             as.matrix lapply rev "rownames<-"
#' @importMethodsFrom S4Vectors t grepl
#' @importMethodsFrom SummarizedExperiment cbind order
#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_rand_strings
#' @importFrom ReactomePA enrichPathway
#' @importFrom edgeR calcNormFactors equalizeLibSizes DGEList
#' @importFrom DESeq2 DESeq results DESeqDataSetFromMatrix
#' @importFrom annotate geneSymbols
#' @importFrom reshape2 melt
#' @import org.Hs.eg.db
#' @import org.Mm.eg.db
deServer <- function(input, output, session) {
    tryCatch(
    {
        if (!interactive()) {
            options( shiny.maxRequestSize = 30 * 1024 ^ 2,
                shiny.fullstacktrace = TRUE, shiny.trace=TRUE, 
                shiny.autoreload=TRUE)
            #library(debrowser)
        }
        observeEvent(input$stopApp, {
            stopApp(returnValue = invisible())
        })
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
            getQCPanel(input)
        })
        output$intheatmap <- renderUI({
            getIntHeatmapVis(randstr())
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
            a <- getDownloadSection(TRUE, "QC")
            if (!is.null(input$startDESeq) && input$startDESeq &&
                !is.null(comparison()$init_data))
                a <- getDownloadSection(!is.null(comparison()$init_data), 
                    "main")
            a
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
            hide(id = "loading-debrowser", anim = TRUE, animType = "fade")    
            return(!is.null(Dataset()))
        })
        outputOptions(output, "dataready", 
            suspendWhenHidden = FALSE)
        
        output$definished <- reactive({
            return(!is.null(filt_data()))
        })
        outputOptions(output, "definished", 
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
            togglePanels(2, c(2, 4), session)
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
            if (!is.null(comparison()$init_data))
                comparison()$init_data 
            else
                qcdata()
        })
        filt_data <- reactive({
            if (!is.null(comparison()$init_data) &&
                !is.null(input$padjtxt) &&
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
            setFilterParams(session, input)
        })
        condmsg <- reactiveValues(text = NULL)
        observeEvent(input$startPlots, {
            compselect <- 1
            if (!is.null(input$compselect) ) 
                compselect <- as.integer(input$compselect)
            if (!is.null(isolate(filt_data())) && !is.null(input$padjtxt) && 
                !is.null(input$foldChangetxt)) {
                condmsg$text <- getCondMsg( isolate(cols()), isolate(conds()))
                selected$data <- getMainPanelPlots(isolate(filt_data()), 
                    isolate(cols()), isolate(conds()), input, compselect)
            }
        })
        qcdata <- reactive({
            prepDataForQC(Dataset()[,input$samples])
        })
        heatdat <- reactive({
            if (is.null(heatmapVals$data))
                heatmapVals$data <- getQCReplot(cols(), conds(), 
                    datasetInput(), input, inputQCPlot())
            dat <- heatmapVals$data
            if (is.null(dat)) return (NULL)
            count = nrow(t(dat$carpet))
            dat <- reshape2::melt(t(dat$carpet), 
            varnames=c("Genes","Samples"), value.name="Values")
            ID <- paste0(dat$Genes, "_", dat$Samples)
            dat <- cbind(dat, ID)
            rownames(dat) <- dat$ID
            list(dat, count)
        })
        observe({
            if (inputQCPlot()$interactive == 1 && input$qcplot == "heatmap")
                selected$data <- 
                    getSelHeat(isolate(init_data()), isolate(heatdat()[[1]]),
                        isolate(heatdat()[[2]])) 
        })

        heatmapVals <- reactiveValues(data = NULL)
   
        output$qcplotout <- renderPlot({
            heatmapVals$data <- getQCReplot(cols(), conds(), 
                 datasetInput(), input, inputQCPlot())
            return( heatmapVals$data )
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
            distance_method = "cor", interactive = FALSE, width = 700, height = 500)
        inputQCPlot <- eventReactive(input$startQCPlot, {
            m <- c()
            m$clustering_method <- input$clustering_method
            m$distance_method <- input$distance_method
            m$interactive <- input$interactive
            m$width <- input$width
            m$height <- input$height
            return(m)
        })
        inputGOstart <- eventReactive(input$startGO, {
            return(getGOPlots(isolate(datasetInput())[, isolate(cols())],
                input, table = FALSE))
        })
        output$GOPlots1 <- renderPlot({
            inputGOstart()
        })
      
        output$tables <- DT::renderDataTable({
            dat <- getDataForTables(input, init_data(),
                  filt_data(), selected,
                  getMostVaried(),  isolate(mergedComp()))
            dat2 <- removeCols(c("ID", "x", "y","Legend", "Size"), dat[[1]])
            m <- DT::datatable(dat2,
            options = list(lengthMenu = list(c(10, 25, 50, 100),
            c("10", "25", "50", "100")),
            pageLength = 25, paging = TRUE, searching = TRUE)) %>%
            getTableStyle(input, dat[[2]], dat[[3]])
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
                a <- filt_data()[filt_data()$Legend=="MV" | 
                                 filt_data()$Legend=="GS", ]
            else
                a <- getMostVariedList(data.frame(init_data()), 
                c(input$samples), input$topn, input$mincount)
        a
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

        mergedComp <- reactive({
            dat <- applyFiltersToMergedComparison(
                isolate(mergedCompInit()), choicecounter$nc, input)
            ret <- dat[dat$Legend == "Sig", ]
            #ret[ret$Legend == "Sig", ] <- NULL
            ret
        })
        
        mergedCompInit <- reactive({
            merged <- getMergedComparison(
                isolate(Dataset()), isolate(dc()), choicecounter$nc, input)
            merged
        })
        datasetInput <- function(addIdFlag = FALSE){
            m <- NULL
            if (!input$goQCplots ) {
                mergedCompDat <- NULL
                if (input$dataset == "comparisons")
                    mergedCompDat <- isolate(mergedComp())
                m <- getSelectedDatasetInput(filt_data(), 
                    selected$data$getSelected(), getMostVaried(),
                    mergedCompDat, input)
            }
            else
                m <- getSelectedDatasetInput(init_data(), 
                    getMostVaried = getMostVaried(),
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
