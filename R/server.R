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
#'             updateTabsetPanel validate wellPanel tags h4 isolate
#'             shinyServer
#' @importFrom DT datatable dataTableOutput renderDataTable
#' @importFrom ggplot2 aes_string geom_point ggplot labs
#'             scale_x_discrete scale_y_discrete
#' @importFrom ggvis add_axis add_legend add_tooltip axis_props
#'             bind_shiny create_broker ggvis ggvisOutput handle_brush
#'             hide_legend layer_bars layer_boxplots layer_points
#'             scale_nominal set_options %>% group_by
#' @importFrom igraph layout.kamada.kawai  
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics barplot hist pairs par rect text
#' @importFrom stats aggregate as.dist cor cor.test dist
#'             hclust kmeans na.omit prcomp var
#' @importFrom utils read.table write.table
#' @importMethodsFrom AnnotationDbi as.data.frame as.list colnames
#'             head mappedkeys nrow subset
#' @importMethodsFrom GenomicRanges as.factor
#' @importMethodsFrom IRanges as.matrix "colnames<-" mean
#'             nchar paste rownames toupper unique which
#' @importMethodsFrom S4Vectors t
#' @importMethodsFrom SummarizedExperiment cbind
#' @import ReactomePA
#' @import DOSE
#options( shiny.maxRequestSize = 30 * 1024 ^ 2)
#library("debrowser")

deServer <- function(input, output, session) {

    output$mainpanel <- renderUI({
        a <- NULL
        if (!is.null(filt_data()))
            a <- getMainPanel()
        a
    })
    output$addpanel <- renderUI({
        a <- NULL
        if (!is.null(filt_data()))
            a <- getAddPanel()
        a
    })
    output$gopanel <- renderUI({
        a <- NULL
        if (!is.null(filt_data()))
            a <- getGoPanel()
        a
    })
    output$downloadSection <- renderUI({
        a <- NULL
        if (!is.null(filt_data()))
            a <- getDownloadSection()
        a
    })
    output$leftMenu <- renderUI({
        a <- NULL
        if (!is.null(filt_data()))
            a <- getLeftMenu()
        a
    })
    output$plotarea <- renderUI({
        a <- NULL
        if (!is.null(filt_data()))
            a <- column(12, plotOutput("addplot1",
                                    height = input$height,
                                    width = input$width))
    a
    })
    
    output$loading <- renderUI({
        addResourcePath(prefix = "www", directoryPath =
                        system.file("extdata", "www", 
                                    package = "debrowser"))
        imgsrc <- "www/images/loading.gif"
        a<-list(
        tags$head(tags$style(type = "text/css", "
            #loadmessage {
                        position: fixed;
                        top: 0px;
                        left: 200px;
                        width: 70%;
                        height: 100;
                        padding: 5px 0px 5px 0px;
                        text-align: center;
                        font-weight: bold;
                        font-size: 100%;
                        color: #000000;
                        opacity: 0.8;
                        background-color: #FFFFFFF;
                        z-index: 100;
            }")),
        conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                        tags$div("Please wait! Loading...", id = "loadmessage",
                            tags$img(src = imgsrc
                            ))))
    })

    output$startup <- renderUI({
        a <- list( column( 12, wellPanel(
        helpText("Please select a file or
                    load the demo data!"),
        helpText( "For mor information;" ),
        helpText(   a("Quick Start Guide",
    href = "http://dolphin.readthedocs.org/en/master/debrowser/quickstart.html",
                    target = "_blank")
                    ) ) ))
    })
    output$afterload <- renderUI({
    a <- list( column( 12, wellPanel(
        helpText( "Please choose the appropriate conditions for DESeq analysis
                and press 'Run DESeq!' button in the left menu" ),
        helpText( "To be able to select conditions please click
                'Condition1' or 'Condition2' boxes.
                You can also use delete button to remove the
                samples from the list."))))
    })
    output$fileUploaded <- reactive({
        return(!is.null(Dataset()))
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

    Dataset <- reactive({
        if (is.null(input$file1) && is.null(loaddemo()$demo)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        else if (is.null(input$file1) && loaddemo()$demo == TRUE) {
            return(loaddemo()$demodata)
        }
        inFile <- input$file1
        validate(need(try(m <- read.table(inFile$datapath, sep = "\t",
                        header = TRUE, row.names = 1)),
                        "Please select a data set"))
        m
    })
    loaddemo <- reactiveValues(demo = NULL, demodata = NULL)
    loaddemo <- eventReactive(input$demo, {
        m <- c()
        m$demo <- TRUE
        load(system.file("extdata", "demo", "demodata.Rda",
                        package = "debrowser"))
        m$demodata <- demodata
        m
    })

    selected1 <- reactive({
        Dataset() %>% getSampleNames(1)
    })

    selected2 <- reactive({
        Dataset() %>% getSampleNames(2)
    })

    choices <- reactive({
        m <- NULL
        if (!is.null(Dataset())){
            cnames <- colnames(Dataset())
            cn <- cnames[2:length(cnames)]
            m <- as.list(NULL)
            for (i in seq(cn)) {
                m[i] <- cn[i]
            }
        }
        m
    })

    observe({
        if (!is.null(input$addplot)) {
            updateRadioButtons(session, "addplot", selected = input$addplot)
        }
    })

    output$condition1Selector <- renderUI({
        selectInput("condition1", label = "Condition 1",
            choices = choices(), multiple = TRUE,
                selected = selected1())
    })
    output$condition2Selector <- renderUI({
        selectInput("condition2", label = "Condition 2",
            choices = choices(), multiple = TRUE,
                selected = selected2())
    })

    inputconds <- reactiveValues(conds1 = NULL, conds2 = NULL)
    inputconds <- eventReactive(input$goButton, {
        m <- c()
        validate(need(input$condition1, "Condition1 has to be selected"),
            need(input$condition2, "Condition2 has to be selected"))
        m$conds1 <- input$condition1
        m$conds2 <- input$condition2
        m$fittype <- input$fittype
        return(m)
    })

    conds <- reactive({
        m <- c(rep(x, length(inputconds()$conds1)), rep(y,
            length(inputconds()$conds2)))
        m
    })

    columns <- reactive({
        m <- c(paste(inputconds()$conds1), paste(inputconds()$conds2))
    })

    avgdata <- reactive({
        m <- as.data.frame(filt_data())
        m <- m[, c(paste(inputconds()$conds1), paste(inputconds()$conds2))]
        m
    })

    filt_data <- reactive({
        data <- Dataset()[, columns()]
        if (length(columns) == length(conds))
            de_res <- runDESeq(data, columns(), conds(), inputconds()$fittype,
                non_expressed_cutoff = 10)
        de_res %>% head
        de_res <- data.frame(de_res)
        norm_data <- getNormalizedMatrix(data[, columns()])
        if (length(inputconds()$conds1) > 1)
            mean_cond1 <- rowMeans(norm_data[rownames(de_res),
                            paste(inputconds()$conds1)])
        else
            mean_cond1 <- norm_data[rownames(de_res),
                            paste(inputconds()$conds1)]

        if (length(inputconds()$conds2) > 1)
            mean_cond2 <- rowMeans( norm_data[ rownames( de_res ),
                            paste( inputconds()$conds2 )] )
        else
            mean_cond2 <- norm_data[ rownames( de_res ),
                            paste( inputconds()$conds2 )]

        m <- cbind(rownames(de_res), norm_data[rownames(de_res), columns()],
            log10(mean_cond1 + 0.1),
            log10(mean_cond2 + 0.1),
                    de_res[rownames(de_res),
            c("padj", "log2FoldChange")], 2 ^ de_res[rownames(de_res),
                "log2FoldChange"],
            -1 * log10(de_res[rownames(de_res), "padj"]))
            colnames(m) <- c("ID", columns(), x, y, "padj", "log2FoldChange",
                "foldChange", "log10padj")
        m <- as.data.frame(m)
        m$padj[is.na(m$padj)] <- 1
        m
    })

    rdata <- reactive({
        if (is.null(input$padj)) {
            padj_cutoff <- 0.01
        } else {
            padj_cutoff <- input$padj
        }
        if (is.null(input$foldChange)) {
            foldChange_cutoff <- 2
        } else {
            foldChange_cutoff <- input$foldChange
        }
        m <- filt_data()
        # Add column which says whether a gene significant or not
        m$Legend <- character(nrow(m))
        m$Legend[m$log2FoldChange > log2(foldChange_cutoff) &
            m$padj < padj_cutoff] <- "Up"
        m$Legend[m$log2FoldChange < log2(1 / foldChange_cutoff) &
            m$padj < padj_cutoff] <- "Down"
        m$Legend[abs(m$log2FoldChange) <= log2(foldChange_cutoff)] <- "NS"
        m$Legend[is.null(m$log10padj)] <- "NA"
        m
    })

    # Function for generating tooltip text
    data_tooltip <- function(dd) {
        if (is.null(dd))
            return(NULL)
        if (is.null(dd$ID))
            return(NULL)
        f <- filt_data()
        # Pick out the movie with this ID
        dat <- f[f$ID == dd$ID, ]

        bardata <- as.data.frame(cbind(columns(),
            t(dat[, columns()]), conds()))

        colnames(bardata) <- c("libs", "count", "conds")

        ypos <- -5 * max(nchar(columns()))
        bardata$count <- as.numeric(as.character(bardata$count))

        title3 <- paste(dat$ID, " variation")
        title4 <- paste(dat$ID, " conditions")

        vis3 <- bardata %>%
            ggvis(x = ~libs, y = ~count,
                fill = ~conds) %>%
            group_by(conds) %>% layer_bars() %>%
            add_title_pos(title = title3, angle = 310,
                dy = ypos, dx = 0) %>%
            set_options(width = 400, height = 350)
        vis3 %>% bind_shiny("plot3")

        vis4 <- bardata %>%
            ggvis(x = ~conds, y = ~count,
                fill = ~conds) %>%
            group_by(conds) %>% layer_boxplots() %>%
            add_title_pos(title = title4, align = "middle") %>%
            set_options(width = 400, height = 350)
        vis4 %>% bind_shiny("plot4")

        paste0(getToolTipText(dat))
    }

    lb_scatter <- linked_brush_scatter()
    lb_volcano <- linked_brush_volcano()
    lb_ma <- linked_brush_ma()

    rd <- reactive({
        rdata()
    })
    gene_selected <- reactive({
        rd()[lb_scatter$selected(), ]
    })

    volcano_dat <- reactive({
        rdata()[which(!is.na(rdata()$log2FoldChange)
            & !is.na(rdata()$log10padj)
            & !is.na(rdata()$Legend)),
        ]
    })

    volcano_dat1 <- reactive({
        volcano_dat()
    })
    volcano_selected <- reactive({
        volcano_dat1()[lb_volcano$selected(), ]
    })

    ma_dat <- reactive({
        dat <- rdata()
        dat$M <- rdata()$Cond1 - rdata()$Cond2
        dat$A <- (rdata()$Cond1 + rdata()$Cond2) / 2
        return(dat)
    })

    ma_dat1 <- reactive({
        ma_dat()
    })
    ma_selected <- reactive({
        ma_dat1()[lb_ma$selected(), ]
    })

    observe({
        if (is.null(Dataset()) ||
            is.null(inputconds()$conds1) ||
            is.null(inputconds()$conds2)) {
            return(NULL)
        }
        volcano_dat1 %>% volcanoPlot(lb_volcano, data_tooltip) %>%
            bind_shiny("vplot1")
        volcano_selected %>% volcanoZoom(data_tooltip) %>%
            bind_shiny("vplot2")

        ma_dat1 %>% MAPlot(lb_ma, data_tooltip) %>% bind_shiny("vplot3")
        ma_selected %>% MAZoom(data_tooltip) %>% bind_shiny("vplot4")

        rd %>% mainScatter(lb_scatter, data_tooltip) %>% bind_shiny("vplot5")
        gene_selected %>% scatterZoom(data_tooltip) %>% bind_shiny("vplot6")
    })
    observe({
        if (is.null(input$radiotabs))
            return(NULL)
        if (input$addplot == "all2all" ||
            input$addplot == "heatmap" ||
            input$addplot == "pca" ||
            input$radiotabs == "panel2") {
            updateTabsetPanel(session, inputId = "methodtabs",
                selected = "panel2")
        }
    })

    observe({
        if (is.null(input$radiotabs))
            return(NULL)
        if (input$goplot == "enrichGO" ||
            input$goplot == "enrichKEGG" ||
            input$goplot == "compareCluster" ||
            input$goplot == "disease" ||
            input$radiotabs == "panel3") {
            updateTabsetPanel(session, inputId = "methodtabs",
                selected = "panel3")
        }
    })
    observe({
        if (is.null(input$radiotabs))
            return(NULL)
        if (input$mainplot == "volcano" ||
            input$mainplot == "maplot" ||
            input$mainplot == "scatter" ||
            input$radiotabs == "panel1") {
            updateTabsetPanel(session, inputId = "methodtabs",
                selected = "panel1")
        }
    })

    output$addplot1 <- renderPlot({
        a <- NULL
        if (!is.null(input$addplot)) {
            dataset <- datasetInput()[, columns()]
            metadata <- cbind(columns(), conds())
            a <- getAddPlots(dataset, input$dataset, input$addplot, metadata,
                clustering_method = inputAddPlot()$clustering_method,
                distance_method = inputAddPlot()$distance_method,
                cex = input$cex)
        }
        a
    })

    inputAddPlot <- reactiveValues(clustering_method = "ward",
        distance_method = "cor")

    inputAddPlot <- eventReactive(input$startAddPlot, {
        m <- c()
        m$clustering_method <- input$clustering_method
        m$distance_method <- input$distance_method
        return(m)
    })

    inputGO <- reactiveValues(ontology = NULL, gopvalue = NULL,
        goplot = NULL, gofunc = NULL, goextplot = NULL)

    inputGO <- eventReactive(input$startGO, {
        m <- c()
        m$ontology <- input$ontology
        m$gopvalue <- input$gopvalue
        m$goplot <- input$goplot
        m$gofunc <- input$gofunc
        m$goextplot <- input$goextplot
        return(m)
    })

    output$GOPlots1 <- renderPlot({
        a <- NULL
        genelist <- getGeneList(rownames(isolate(datasetInput())))
        a <- getGOPlots(isolate(datasetInput()[, isolate(columns())]),
            inputGO(), genelist)
        a
    })

    output$table <- DT::renderDataTable(if (!is.null(filt_data()))
        DT::datatable(filt_data(), options =
            list(lengthMenu = list(c(5, 15, -1),
                c("5", "15", "All")),
            pageLength = 15, paging = TRUE, searching = TRUE)))
    output$up <- DT::renderDataTable(if (!is.null(filt_data()))
        DT::datatable(rdata()[rdata()[, "Legend"] == "Up", ], options =
            list(lengthMenu = list(c(5, 15, -1),
                c("5", "15", "All")),
            pageLength = 15, paging = TRUE, searching = TRUE)))
    output$down <- DT::renderDataTable(if (!is.null(filt_data()))
        DT::datatable(rdata()[rdata()[, "Legend"] == "Down", ], options =
            list(lengthMenu = list(c(5, 15, -1),
                c("5", "15", "All")),
            pageLength = 15, paging = TRUE, searching = TRUE)))

    getSelected <- reactive({
        m <- c()
        if (input$mainplot == "volcano") {
            m <- volcano_selected()
        } else if (input$mainplot == "scatter") {
            m <- gene_selected()
        } else if (input$mainplot == "maplot") {
            m <- ma_selected()
        }
        m
    })

    output$selected <- DT::renderDataTable({
        if (!is.null(filt_data()))
            DT::datatable(getSelected(), options =
                list(lengthMenu = list(c(5, 15, -1),
                c("5", "15", "Selected")),
                pageLength = 15, paging = TRUE, searching = TRUE))
    })

    datasetInput <- reactive({
        m <- rdata()
        if (input$dataset == "up") {
            m <- rdata()[rdata()[, "Legend"] == "Up", ]
        } else if (input$dataset == "down") {
            m <- rdata()[rdata()[, "Legend"] == "Down", ]
        } else if (input$dataset == "up+down") {
            m <- rdata()[which(rdata()[, "Legend"] == "Down" |
                rdata()[, "Legend"] == "Up"), ]
        } else if (input$dataset == "alldetected") {
            m <- rdata()
        } else if (input$dataset == "selected") {
            m <- getSelected()
        }
        m
    })

    output$downloadData <- downloadHandler(filename = function() {
        paste(input$dataset, input$filetype, sep = ".")
    }, content = function(file) {
        write.table(datasetInput(), file, sep = ",", row.names = FALSE)
    })
    output$downloadPlot <- downloadHandler(filename = function() {
        paste(input$addplot, ".pdf", sep = "")
    }, content = function(file) {
        pdf(file, height = input$height * 0.039370,
            width = input$width * 0.039370)
        print( getAddPlots(datasetInput()[, columns()], input$dataset,
            input$addplot, cbind(columns(), conds()),
            clustering_method = inputAddPlot()$clustering_method,
            distance_method = inputAddPlot()$distance_method,
            cex = input$cex) )
        dev.off()
    })

    output$downloadGOPlot <- downloadHandler(filename = function() {
        paste(input$goplot, ".pdf", sep = "")
    }, content = function(file) {
        pdf(file)
        print(getGOPlots(datasetInput()[, columns()], inputGO(),
            getGeneList(rownames(datasetInput()))))
        dev.off()
    })
}
