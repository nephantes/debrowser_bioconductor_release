#' deUI
#'
#' Creates a shinyUI to be able to run DEBrowser interactively.
#'
#' @note \code{deUI}
#' @return the panel for main plots;
#'
#' @examples
#'     x<-deUI()
#'
#' @export
#'

deUI <- function() {
    dbHeader <- dashboardHeader(titleWidth = 250)
    dbHeader$children[[2]]$children <- tags$a(style='color: white;',
         id="top_logo" , "DEBrowser")
    addResourcePath(prefix = "www", directoryPath = system.file("extdata",
        "www", package = "debrowser"))
     
    debrowser <- (fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS("
        #loading-debrowser {
        position: absolute;
        background: #000000;
        opacity: 0.9;
        z-index: 100;
        left: 0;
        right: 0;
        height: 100%;
        text-align: center;
        color: #EFEFEF;
    }"),
    # Loading message
    tags$div(h4("Loading DEBrowser"), id = "loading-debrowser",
        tags$img(src = "www/images/initial_loading.gif")),
    tags$head(tags$title("DEBrowser"),
        tags$link(rel = "stylesheet", type = "text/css",
        href = "www/shinydashboard_additional.css")
    ),
    dashboardPage(
        dbHeader,
        dashboardSidebar(
            width = 250,
            getJSLine(),
                uiOutput("loading"),
                tabsetPanel(id = "menutabs", type = "tabs",
                tabPanel(title = "Data Prep", value = "dataprep", id="dataprep",
                sidebarMenu(id="DataPrep",
                    menuItem("Upload", tabName = "Upload"),
                    menuItem("Filter", tabName = "Filter"),
                    menuItem("BatchEffect", tabName = "BatchEffect"),
                    menuItem("CondSelect", tabName = "CondSelect"),
                    menuItem("DEAnalysis", tabName = "DEAnalysis"),
                    menuItem("DEFilter", tabName = "DEAnalysis",  startExpanded = TRUE,
                             uiOutput("cutOffUI"),
                             uiOutput("compselectUI"))
                ),p("Logged in as: ", textOutput("user_name"))),
                tabPanel(title = "Discover", value = "discover", id="discover",
                conditionalPanel(condition = "(output.dataready)",
                    conditionalPanel( (condition <- "input.methodtabs=='panel1'"),
                    mainPlotControlsUI("main")),
                    uiOutput("downloadSection"),
                    uiOutput('cutoffSelection'),
                    uiOutput("leftMenu"))
                 ))
        ),
    dashboardBody(
        mainPanel(
            width = 12,
            tags$head(
                tags$style(type = "text/css",
                        "#methodtabs.nav-tabs {font-size: 14px} ")),
                tabsetPanel(id = "methodtabs", type = "tabs",
                    tabPanel(title = "Data Prep", value = "panel0", id="panel0",
                             tabItems(
                                 tabItem(tabName="Upload", dataLoadUI("load")),
                                 tabItem(tabName="Filter",
                                         conditionalPanel(
                                             (condition <- "input.Filter"),
                                         dataLCFUI("lcf"))),
                                 tabItem(tabName="BatchEffect", 
                                         conditionalPanel(
                                             (condition <- "input.Batch"),
                                         batchEffectUI("batcheffect"))),
                                 tabItem(tabName="CondSelect", 
                                         conditionalPanel(
                                             (condition <- "input.goDE"),
                                         condSelectUI())),
                                 tabItem(tabName="DEAnalysis", 
                                         conditionalPanel(
                                             (condition <- "input.goDE"),
                                         uiOutput("deresUI")))
                             )),
                    tabPanel(title = "Main Plots", value = "panel1", id="panel1",
                            uiOutput("mainmsgs"),
                            uiOutput("mainpanel")),
                    tabPanel(title = "QC Plots", value = "panel2", id="panel2",
                            uiOutput("qcpanel")),
                    tabPanel(title = "GO Term", value = "panel3", id="panel3",
                            uiOutput("gopanel")),
                    tabPanel(title = "Tables", value = "panel4", id="panel4",
                            dataTableOutput("tables")))
        ),
        getTabUpdateJS()
        ))
    )
    )
    debrowser
}
