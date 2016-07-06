#' getDataPrepPanel
#'
#' Create and show the Condition selection screen to the user
#' within the DEBrowser.
#'
#' @param flag, flag to show the element in the ui
#' @note \code{getDataPrepPanel}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getDataPrepPanel()
#' @export
#'
getDataPrepPanel <- function(flag = FALSE){
    a <- NULL
    if(flag)  
    a<- list(
        conditionalPanel(condition = "input.demo ||
            output.dataready",
        wellPanel(
        uiOutput("sampleSelector"),
        actionButton("startDESeq", "Go to DE Analysis!"),
        actionButton("goQCplots", "Go to QC plots!"),
        actionButton("resetsamples", "Reset Samples!"),
        conditionalPanel(condition = "input.startDESeq",
            helpText( "Please add  new comparisons for DE analysis!" ),
            uiOutput("conditionSelector"),
            actionButton("add_btn", "Add New Comparison"),
            actionButton("rm_btn", "Remove"),
            selectInput("fittype",
                label = "Fit Type:",
                choices = list ("parametric" = "parametric",
                "local" = "local", "mean" = "mean"),
                selected = 1,
                multiple = FALSE
            ),
        actionButton("goButton", "Submit!")))),
        conditionalPanel(condition = "!input.demo &&
            !output.fileUploaded",
            uiOutput("startup"))
    )
    a
}

#' getLeftMenu
#'
#' Generates the left menu for for plots within the DEBrowser.
#'
#' @param flag, flag to show the element in the ui
#' @note \code{getLeftMenu}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getLeftMenu()
#' @export
#'
getLeftMenu <- function(flag = TRUE) {
a <- NULL
if(flag)
a <- list( conditionalPanel( (condition <- "input.methodtabs=='panel1'"),
        wellPanel(radioButtons("mainplot", paste("Main Plots:", sep = ""),
            c(Scatter = "scatter", VolcanoPlot = "volcano",
            MAPlot = "maplot"))), actionButton("startPlots", "Submit!")),
        conditionalPanel( (condition <- "input.methodtabs=='panel2'"),
            wellPanel(radioButtons("qcplot",
                paste("QC Plots:", sep = ""),
                c(All2All = "all2all", Heatmap = "heatmap", PCA = "pca"))),
            getQCLeftMenu()),
        conditionalPanel( (condition <- "input.methodtabs=='panel3'"),
            wellPanel(radioButtons("goplot", paste("Go Plots:", sep = ""),
                c(enrichGO = "enrichGO", enrichKEGG = "enrichKEGG",
                Disease = "disease", compareClusters = "compare"))),
                getGOLeftMenu()
                ))
a
}

#' getGOLeftMenu
#'
#' Generates the GO Left menu to be displayed within the DEBrowser.
#'
#' @note \code{getGOLeftMenu}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getGOLeftMenu()
#' @export
#'
getGOLeftMenu <- function() {
    a <- list(
    tags$head(tags$script(HTML(logSliderJScode("gopvalue")))),
    sliderInput("gopvalue", "p.adjust cut off",
        min=0, max=10, value=6, sep = "", 
        animate = FALSE),
    textInput("pvaluetxt", "or p.adjust", value = "0.01" ),
        getOrganismBox(),
        conditionalPanel( ( condition <- "(input.goplot=='enrichGO' ||
            (input.goplot=='compare' && input.gofunc!='enrichDO' &&
            input.gofunc!='enrichKEGG'))" ),
            selectInput("ontology", "Choose an ontology:",
                choices =  c( "CC", "MF", "BP"))
            ),
            conditionalPanel( ( condition <- "input.goplot!='compare'"),
                selectInput("goextplot", "Plot Type:",
                choices =  c("Summary", "Dotplot"))
            ),
            conditionalPanel( ( condition <- "input.goplot=='compare'"),
                selectInput("gofunc", "Plot Function:",
                choices =  c( "enrichGO", "enrichDO", "enrichPathway",
                "enrichKEGG"))
            ),
            actionButton("startGO", "Submit"),
            downloadButton("downloadGOPlot", "Download Plots"))
}

#' getPCselection
#'
#' Generates the PC selection number to be used within DEBrowser.
#'
#' @param num, PC selection number
#' @param xy, x or y coordinate
#' @note \code{getPCselection}
#' @return PC selection for PCA analysis
#' @examples
#'     x <- getPCselection()
#' @export
#'
getPCselection <- function(num = 1, xy = "x" ) {
    numericInput(paste0("pcsel", xy), 
        paste0("PC selection[", xy, "]"), num, 1, 6)
}

#' getQCLeftMenu
#'
#' Generates the left menu to be used for QC plots within the
#' DEBrowser.
#'
#' @note \code{getQCLeftMenu}
#' @return QC left menu
#' @examples
#'     x <- getQCLeftMenu()
#' @export
#'
getQCLeftMenu <- function() {
    a <- list(conditionalPanel( (condition <- "input.qcplot=='all2all' ||
            input.qcplot=='heatmap' ||
            input.qcplot=='pca'"),
            sliderInput("width", "width",
            min = 100, max = 2000, step = 10, value = 700),
            sliderInput("height", "height",
            min = 100, max = 2000, step = 10, value = 500),
            conditionalPanel( (condition <- "input.qcplot=='all2all'"),
                sliderInput("cex", "corr font size",
                min = 0.1, max = 10,
                step = 0.1, value = 2)),
            conditionalPanel( (condition <- "input.qcplot=='heatmap'"),
                checkboxInput("interactive", "Interactive", value = FALSE),
                selectInput("clustering_method", "Clustering Method:",
                choices <- c("complete", "ward.D2", "single", "average",
                "mcquitty", "median", "centroid")),
                selectInput("distance_method", "Distance Method:",
                choices <- c("cor", "euclidean", "maximum", "manhattan",
                "canberra", "binary", "minkowski"))),
        actionButton("startQCPlot", "Submit"),
        conditionalPanel( (condition <- "input.qcplot=='pca'"),
            getPCselection(1, "x"),
            getPCselection(2, "y")
        ),
        downloadButton("downloadPlot", "Download Plot")))
}

#' logSliderJScode
#'
#' Generates the log based slider to be used by the user within
#' DEBrowser.
#'
#' @param slidername, id of the slider
#' @note \code{logSliderJScode}
#' @return returns the slider values in log10 scale
#' @examples
#'     x <- logSliderJScode()
#' @export
#'
logSliderJScode <- function(slidername = NULL){
    if (is.null(slidername)) return (NULL)
    a <- paste0("$(function() {
    setTimeout(function(){
    var vals = [0];
    var powStart = 4;
    var powStop = 0;
    for (i = powStart; i >= powStop; i--) {
    var val = Math.pow(10, -i)/2;
    val = parseFloat(val.toFixed(8));
    vals.push(val);
    var val = Math.pow(10, -i);
    val = parseFloat(val.toFixed(8));
    vals.push(val);
    }
    $('#", slidername,"').data('ionRangeSlider').update({'values':vals})
    }, 4)})")
}

#' getCutOffSelection
#'
#' Gathers the cut off selection for DE analysis
#'
#' @param flag, flag to show the element in the ui
#' @param nc, total number of comparisons
#' @note \code{getCutOffSelection}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getCutOffSelection()
#' @export
#'
getCutOffSelection <- function(flag = TRUE, nc = 1){
    a <- NULL
    if (flag) {
        compselect <- getCompSelection(nc)
        a <- list( 
        conditionalPanel( (condition <- "input.dataset!='most-varied'"),
            tags$head(tags$script(HTML(logSliderJScode("padj")))),
            h4("Filter"),
            sliderInput("padj", "padj value cut off",
                min=0, max=10, value=6, sep = "", 
                animate = FALSE),
            textInput("padjtxt", "or padj", value = "0.01" ),
            sliderInput("foldChange", "Fold Change cut off",
                1, 10, 2, step = 0.1),
            textInput("foldChangetxt", "or foldChange", value = "2" )),
            compselect
        )
    }
    a
}

#' getInitialMenu
#'
#' Displays the initial menu within DEBrowser.
#'
#' @param input, input from user
#' @param output, output to user
#' @param session, session info
#' @note \code{getInitialMenu}
#' @return returns the initial menu
#' @examples
#'     x <- getInitialMenu()
#' @export
#'
getInitialMenu <- function(input = NULL, output = NULL, session = NULL) {
    if (is.null(input)) return (NULL)
    a<-NULL
    if (is.null(parseQueryString(session$clientData$url_search)$jsonobject))
    {
        a<-list(
            conditionalPanel(condition = "!input.demo &&
                !output.dataready",
                actionLink("demo", "Load Demo!"),
                fileInput("file1", "Choose TSV File",
                    accept = c("text/tsv",
                        "text/comma-separated-values,text/plain",
                        ".tsv")))
        )
    }
    a
}

#' getProgramTitle
#'
#' Generates the title of the program to be displayed within DEBrowser.
#' If it is called in a program, the program title will be hidden
#'
#' @param session, session var
#' @note \code{getProgramTitle}
#' @return program title
#' @examples
#'     title<-getProgramTitle()
#' @export
#'
getProgramTitle <- function(session = NULL) {
    if (is.null(session)) return (NULL)
    a<-NULL
    title<-parseQueryString(session$clientData$url_search)$title
    if (is.null(title) || title != "no" ) 
        a <- list(titlePanel("DEBrowser"))
    else
        a <- list(titlePanel(" "))
    a
}

#' getLoadingMsg
#'
#' Creates and displays the loading message/gif to be displayed
#' within the DEBrowser.
#'
#' @note \code{getLoadingMsg}
#' @return loading msg
#' @examples
#'     x <- getLoadingMsg()
#' @export
#'
getLoadingMsg <- function() {
    addResourcePath(prefix = "www", directoryPath =
        system.file("extdata", "www", 
        package = "debrowser"))
    imgsrc <- "www/images/loading.gif"
    a <- list(
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
}

#' getLogo
#'
#' Generates and displays the logo to be shown within DEBrowser.
#'
#' @note \code{getLogo}
#' @return return logo
#' @examples
#'     x <- getLogo()
#' @export
#'
getLogo <- function(){
    addResourcePath(prefix = "www", directoryPath =
        system.file("extdata", "www", 
        package = "debrowser"))
    imgsrc <- "www/images/logo.png"
    a<-list(img(src=imgsrc, align = "right"))
}

#' getStartupMsg
#'
#' Generates and displays the starting message within DEBrowser.
#'
#' @note \code{getStartupMsg}
#' @return return startup msg
#' @examples
#'     x <- getStartupMsg()
#' @export
#'
getStartupMsg <- function() {
a <- list( column( 12, wellPanel(
helpText("Please select a file or load the demo data!"),
helpText( "For more information;" ),
helpText(   a("Quick Start Guide",
href = "http://dolphin.readthedocs.org/en/master/debrowser/quickstart.html",
target = "_blank")) ) ))
}

#' getAfterLoadMsg
#'
#' Generates and displays the message to be shown after loading data
#' within the DEBrowser.
#'
#' @note \code{getAfterLoadMsg}
#' @return return After Load Msg
#' @examples
#'     x <- getAfterLoadMsg()
#' @export
#'
getAfterLoadMsg <- function() {
a <- list( column( 12, wellPanel(
helpText( "Please choose the appropriate conditions for DESeq analysis
            and press 'Run DESeq!' button in the left menu" ),
helpText( "To be able to select conditions please click
            'Condition1' or 'Condition2' boxes.
            You can also use delete button to remove the
            samples from the list."))))
}

#' getStartPlotsMsg
#'
#' Generates and displays the starting messgae to be shown once
#' the user has first seen the main plots page within DEBrowser.
#'
#' @note \code{getStartPlotsMsg}
#' @return return start plot msg
#' @examples
#'     x <- getStartPlotsMsg()
#' @export
#'
getStartPlotsMsg <- function() {
a <- list( conditionalPanel(condition <- "!input.startPlots",
    column( 12, wellPanel(
    helpText( "Please choose the appropriate parameters and 
            press submit button to draw the plots!" )))))
}

#' getCondMsg
#'
#' Generates and displays the current conditions and their samples
#' within the DEBrowser.
#'
#' @param cols, columns
#' @param conds, selected conditions
#' @note \code{getCondMsg}
#' @return return conditions
#' @examples
#'     x <- getCondMsg()
#' @export
#'
getCondMsg <- function(cols = NULL, conds = NULL) {
    a <- NULL
    if (!is.null(cols) && !is.null(conds)) {
        cnd <- data.frame(cbind(conds, cols))
        a <-list( conditionalPanel(condition <- "input.startPlots",
            column( 12, wellPanel(
            HTML( paste0( "<b>",unique(conds)[1], ":</b>"), 
                paste(cnd[cnd$conds == unique(conds)[1], "cols"], 
                collapse =","), 
                paste0(" vs. ","<b>",unique(conds)[2], ":", "</b>"),
                paste(cnd[cnd$conds == unique(conds)[2], "cols"], 
                collapse =",")) ))))
    }
    a
}

#' togglePanels
#'
#' User defined toggle to display which panels are to be shown within
#' DEBrowser.
#'
#' @param num, selected panel 
#' @param nums, all panels
#' @param session, session info
#' @note \code{togglePanels}
#' @examples
#'     x <- togglePanels()
#' @export
#'
togglePanels <- function(num = NULL, nums = NULL, session = NULL){
    if (is.null(num)) return (NULL)
    for(i in 0:4){
        if (i %in% nums)
            shinyjs::show(selector =
                paste0("#methodtabs li a[data-value=panel",i,"]"))
        else
            shinyjs::hide(selector = 
                paste0("#methodtabs li a[data-value=panel",i,"]"))
    }
    if(num)
        updateTabsetPanel(session, "methodtabs", 
            selected = paste0("panel", num))
}

#' getCompSelection
#'
#' Gathers the user selected comparison set to be used within the
#' DEBrowser.
#'
#' @param count, comparison count
#' @note \code{getCompSelection}
#' @examples
#'     x <- getCompSelection(count = 2)
#' @export
#'
getCompSelection <- function(count = NULL) {
  a <- NULL
  if (count>1){
        a <- list(selectInput("compselect", 
        label = "Choose a comparison:",
        choices = c(1:count) ))
  }
  a
}

#' getTableStyle
#'
#' User defined selection that selects the style of table to display
#' within the DEBrowser.
#'
#' @param dat, dataset
#' @param input, input params
#' @param padj, the name of the padj value column in the dataset
#' @param foldChange, the name of the foldChange column in the dataset
#' @note \code{getTableStyle}
#' @examples
#'     x <- getTableStyle()
#' @export
#'
getTableStyle <- function(dat = NULL, input = NULL, 
    padj = c("padj"), foldChange=c("foldChange")){
    if (is.null(dat)) return (NULL)
    a <- dat 
    if(!is.null(padj) && padj != "" && !input$goQCplots)
        a <- a %>% formatStyle(
            padj,
            color = styleInterval(c(0, input$padjtxt), 
            c('black', "white", "black")),
            backgroundColor = styleInterval(
            input$padjtxt, c('green', 'white'))
        ) 
    if(!is.null(foldChange) && foldChange != "" && !input$goQCplots)
        a <- a %>% formatStyle(
            foldChange,
            color = styleInterval(c(1/as.numeric(input$foldChangetxt), 
            as.numeric(input$foldChangetxt)), c('white', 'black', 'white')),
            backgroundColor = styleInterval(
            c(1/as.numeric(input$foldChangetxt), 
            as.numeric(input$foldChangetxt)), 
            c('red', 'white', 'green'))
    )
    a
}

#' textareaInput
#'
#' Generates a text area input to be used for gene selection within
#' the DEBrowser.
#'
#' @param id, id of the control
#' @param label, label of the control
#' @param value, initial value
#' @param rows, the # of rows
#' @param cols, the # of  cols
#' @param class, css class
#' @examples
#'     x <- textareaInput("genesetarea", "Gene Set",
#'         "Fgf21", rows = 5, cols = 35)
#' @export
#'
textareaInput <- function(id, label, value, rows=20, cols=35, 
    class="form-control"){
    tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}

#' showObj
#'
#' Displays a shiny object.
#'
#' @param btns, show group of objects with shinyjs
#' @examples
#'     x <- showObj()
#' @export
#'
showObj <- function(btns = NULL) {
    if (is.null(btns)) return (NULL)
    for (btn in seq(1:length(btns)))
        shinyjs::show(btns[btn])
}

#' hideObj
#'
#' Hides a shiny object.
#'
#' @param btns, hide group of objects with shinyjs
#' @examples
#'     x <- hideObj()
#' @export
#'
hideObj <- function(btns = NULL) {
    if (is.null(btns)) return (NULL)
    for (btn in seq(1:length(btns)))
        shinyjs::hide(btns[btn])
}

