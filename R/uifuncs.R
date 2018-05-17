#' getDataPrepPanel
#'
#' Create and show the Condition selection screen to the user
#' within the DEBrowser.
#'
#' @note \code{getDataPrepPanel}
#' @return returns the prep panel;
#' @examples
#'     x <- getDataPrepPanel()
#' @export
#'
getDataPrepPanel <- function(){
    tabItems(
        tabItem(tabName="Upload", dataLoadUI("load"),
                column(4, verbatimTextOutput("loadedtable")
                )),
        tabItem(tabName="Filter",dataLCFUI("lcf"),                
                column(4, verbatimTextOutput("filtertable")
                )),
        tabItem(tabName="BatchEffect", batchEffectUI("batcheffect"),
                column(4, verbatimTextOutput("batcheffecttable")
                ))
    )
}

#' getLeftMenu
#'
#' Generates the left menu for for plots within the DEBrowser.
#'
#' @param input, input values
#' @note \code{getLeftMenu}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getLeftMenu()
#' @export
#'
getLeftMenu <- function(input = NULL) {
if (is.null(input)) return(NULL)
   leftMenu <- list(
        conditionalPanel( (condition <- "input.methodtabs=='panel1'"),
                          actionButton("startPlots", "Submit!"),
        shinydashboard::menuItem(" Plot Type", icon = icon("star-o"), startExpanded=TRUE,
        radioButtons("mainplot", paste("Main Plots:", sep = ""),
            c(Scatter = "scatter", VolcanoPlot = "volcano",
            MAPlot = "maplot"))
                ),
            getMainPlotsLeftMenu()),
        conditionalPanel( (condition <- "input.methodtabs=='panel2'"),
                          
            conditionalPanel( condition <- "input.qcplot=='heatmap'",
                actionButton("startQCPlot", "Submit!")),
                          
        shinydashboard::menuItem(" Plot Type", icon = icon("star-o"), startExpanded = TRUE,
        wellPanel(radioButtons("qcplot",
                paste("QC Plots:", sep = ""),
                c(PCA = "pca", All2All = "all2all", Heatmap = "heatmap", IQR = "IQR",
                  Density = "Density")))),
            getQCLeftMenu(input)),
        conditionalPanel( (condition <- "input.methodtabs=='panel3'"),
            actionButton("startGO", "Submit!"),
        shinydashboard::menuItem(" Plot Type", icon = icon("star-o"), startExpanded = TRUE,
            wellPanel(radioButtons("goplot", paste("Go Plots:", sep = ""),
                c(enrichGO = "enrichGO", enrichKEGG = "enrichKEGG",
                Disease = "disease", compareClusters = "compare")))),
                getGOLeftMenu()
                ),
        conditionalPanel( (condition <- "input.methodtabs=='panel4'"),
        shinydashboard::menuItem(" Select Columns", icon = icon("star-o"), startExpanded=TRUE,
             uiOutput("getColumnsForTables")
        ))
    )
   return(leftMenu)
}
#' getMainPlotsLeftMenu
#'
#' Generates the Main PLots Left menu to be displayed within the DEBrowser.
#'
#' @note \code{getMainPlotsLeftMenu}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getMainPlotsLeftMenu()
#' @export
#'
getMainPlotsLeftMenu <- function() {
    mainPlotsLeftMenu <- list(
        shinydashboard::menuItem(" Background Selection", icon = icon("star-o"),
        sliderInput("backperc", "Background Data(%):",
            min=10, max=100, value=10, sep = "",
            animate = FALSE))
        )
    return(mainPlotsLeftMenu)
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
    list(
    shinydashboard::menuItem(" Go Term Options", icon = icon("star-o"), startExpanded=TRUE, 
                                       
    tags$head(tags$script(HTML(logSliderJScode("gopvalue")))),
    sliderInput("gopvalue", "p.adjust cut off",
        min=0, max=10, value=6, sep = "",
        animate = FALSE),
    textInput("pvaluetxt", "or p.adjust", value = "0.01" ),
        getOrganismBox(),
        #actionButton("KeggPathway", "KeggPathway", style="text-align:center;color: #0000ff; font-size:120%"),
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
                choices =  c( "enrichGO", "enrichDO", "enrichKEGG"))
            ),
            downloadButton("downloadGOPlot", "Download Plots"))
    )

}

#' getQCLeftMenu
#'
#' Generates the left menu to be used for QC plots within the
#' DEBrowser.
#'
#' @param input, input values
#' @note \code{getQCLeftMenu}
#' @return QC left menu
#' @examples
#'     x <- getQCLeftMenu()
#' @export
#'
getQCLeftMenu <- function( input = NULL) {
    if (is.null(input)) return(NULL)
        list(
        shinydashboard::menuItem(" Select Columns", icon = icon("star-o"), startExpanded=TRUE, 
            uiOutput("columnSelForQC")),
            shinydashboard::menuItem(" QC Options", icon = icon("star-o"), startExpanded=FALSE,
            conditionalPanel( (condition <- "input.qcplot=='heatmap'"),
                plotSizeMarginsUI("heatmap"),
                heatmapControlsUI("heatmap")),
            conditionalPanel( condition <- "(input.qcplot=='all2all')",
            sliderInput("width", "width",
            min = 100, max = 2000, step = 10, value = 700),
            sliderInput("height", "height",
            min = 100, max = 2000, step = 10, value = 500)),
            conditionalPanel( (condition <- "input.qcplot=='all2all'"),
                sliderInput("cex", "corr font size",
                min = 0.1, max = 10,
                step = 0.1, value = 2)),
                getHelpButton("method",
                              "http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#heat-maps"),
        conditionalPanel( (condition <- "input.qcplot=='pca'"),
            shinydashboard::menuItem("PCA Options",
            pcaPlotControlsUI("qcpca")),
            plotSizeMarginsUI("qcpca", w=600, h=400, t=50, b=50, l=60, r=0)
        ),
        downloadButton("downloadPlot", "Download Plot"))
    )
}

#' getCutOffSelection
#'
#' Gathers the cut off selection for DE analysis
#'
#' @param nc, total number of comparisons
#' @note \code{getCutOffSelection}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getCutOffSelection()
#' @export
#'
getCutOffSelection <- function(nc = 1){
    compselect <- getCompSelection(nc)
    list( conditionalPanel( (condition <- "input.dataset!='most-varied' &&
        input.methodtabs!='panel0'"),
        tags$head(tags$script(HTML(logSliderJScode("padj")))),
        shinydashboard::menuItem(" Filter", icon = icon("star-o"),
            #h4("Filter"),
            sliderInput("padj", "padj value cut off",
                min=0, max=10, value=6, sep = "",
                animate = FALSE),
            textInput("padjtxt", "or padj", value = "0.01" ),
            sliderInput("foldChange", "Fold Change cut off",
                1, 10, 2, step = 0.1),
            textInput("foldChangetxt", "or foldChange", value = "2" ),
            compselect
        )
    ) )
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
    DEBrowser <- NULL
    title<-parseQueryString(session$clientData$url_search)$title
    if (is.null(title) || title != "no" )
        DEBrowser <- list(titlePanel("DEBrowser"))
    else
        DEBrowser <- list(titlePanel(" "))
    return(DEBrowser)
}

#' getLoadingMsg
#'
#' Creates and displays the loading message/gif to be displayed
#' within the DEBrowser.
#'
#' @param output, output message
#' @note \code{getLoadingMsg}
#' @return loading msg
#' @examples
#'     x <- getLoadingMsg()
#' @export
#'
getLoadingMsg <- function(output = NULL) {
    addResourcePath(prefix = "www", directoryPath =
        system.file("extdata", "www",
        package = "debrowser"))
    imgsrc_full <- "www/images/loading_start.gif"
    imgsrc_small <- "www/images/loading.gif"
    a <- list(
        tags$head(tags$style(type = "text/css", "
            #loadmessage {
            position: fixed;
            top: 0px;
            left: 0px;
            width: 100%;
            height: 100%;
            padding: 5px 0px 5px 0px;
            text-align: center;
            font-weight: bold;
            font-size: 100%;
            color: #000000;
            opacity: 0.8;
            z-index: 100;
            }
            #loadmessage_small {
            position: fixed;
            left: 50%;
            transform: translateX(-50%);
            top: 50px;
            text-align: center;
            opacity: 0.8;
            z-index: 999999;
            }
                             ")),
        conditionalPanel(condition = paste0("$('html').hasClass('shiny-busy')",
            "& input.startDE & input.methodtabs=='panel0'"),
            tags$div(id = "loadmessage",
            tags$img(src = imgsrc_full
            ))),
        conditionalPanel(condition =  paste0("$('html').hasClass('shiny-busy')",
                "& !(input.startDE & input.methodtabs=='panel0')"),
            tags$div(id = "loadmessage_small",
            tags$img(src = imgsrc_small
            )))
        )
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
a <- list( column( 12, 
helpText("Please select a file or load the demo data!"),
helpText( "For more information;" ),
helpText(   a("Quick Start Guide",
href = "http://debrowser.readthedocs.org",
target = "_blank"),
getHelpButton("method", "http://debrowser.readthedocs.org")) ))
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
    column( 12, 
    helpText( "Please choose the appropriate parameters and
            press submit button to draw the plots!" ),
    getHelpButton("method", "http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#the-main-plots"))))
}

#' getCondMsg
#'
#' Generates and displays the current conditions and their samples
#' within the DEBrowser.
#'
#' @param dc, columns
#' @param num, selected comparison
#' @param cols, columns
#' @param conds, selected conditions
#' @note \code{getCondMsg}
#' @return return conditions
#' @examples
#'     x <- getCondMsg()
#' @export
#'
getCondMsg <- function(dc = NULL, num = NULL, cols = NULL, conds = NULL) {
    if (is.null(cols) || is.null(conds)) return (NULL)
    if (is.null(num)) num <- 1
    cnd <- data.frame(cbind(conds, cols))
    params_str <- paste(dc[[as.numeric(num)]]$demethod_params, collapse = ',')
    a <-list( conditionalPanel(condition <- "input.startPlots",
        column( 12, wellPanel(
            style = "overflow-x:scroll",
            HTML( paste0( "<b>Selected Parameters:</b> ", params_str,
            "</br><b>",unique(conds)[1], ":</b> "),
            paste(cnd[cnd$conds == unique(conds)[1], "cols"],
            collapse =","),
            paste0(" vs. ","<b>",unique(conds)[2], ":", "</b> "),
            paste(cnd[cnd$conds == unique(conds)[2], "cols"],
            collapse =",")),
        getHelpButton("method",
"http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#the-main-plots")))))
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


#' getTableStyle
#'
#' User defined selection that selects the style of table to display
#' within the DEBrowser.
#'
#' @param dat, dataset
#' @param input, input params
#' @param padj, the name of the padj value column in the dataset
#' @param foldChange, the name of the foldChange column in the dataset
#' @param DEsection, if it is in DESection or not
#' @note \code{getTableStyle}
#' @examples
#'     x <- getTableStyle()
#' @export
#'
getTableStyle <- function(dat = NULL, input = NULL,
    padj = c("padj"), foldChange=c("foldChange"), DEsection = TRUE){
    if (is.null(dat)) return (NULL)
    a <- dat
    if(!is.null(padj) && padj != "" && DEsection)
        a <- a %>% formatStyle(
            padj,
            color = styleInterval(c(0, input$padjtxt),
            c('black', "white", "black")),
            backgroundColor = styleInterval(
            input$padjtxt, c('green', 'white'))
        )
    if(!is.null(foldChange) && foldChange != "" && DEsection)
        a <- a %>%
            formatStyle(
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

#' getKEGGModal
#' prepares a helpbutton for to go to a specific site in the documentation
#'
#' @return the info button
#'
#' @examples
#'     x<- getKEGGModal()
#'
#' @export
getKEGGModal<-function(){
    a <- bsModal("modalExample", "KEGG Pathway", "KeggPathway", size = "large",
    div(style = "display:block;overflow-y:auto; overflow-x:auto;",imageOutput("KEGGPlot")))
}
