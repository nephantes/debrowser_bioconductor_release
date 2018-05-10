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
    getUrlJSCode <- '
        shinyjs.setButtonHref = function(params) {
            var current_url = window.location.href.split(\"?\")[0];
            top_logo.href = current_url + "?start=true";
            document.getElementsByClassName("fa fa-sign-out")[0].parentElement.setAttribute("href", current_url + "?logout=true");
            document.getElementsByClassName("fa fa-refresh")[0].parentElement.setAttribute("href", current_url + "?start=true");
            document.getElementsByClassName("header")[0].innerHTML = "";
            document.getElementsByClassName("label-primary")[0].innerHTML = "";
        }
        shinyjs.hideDropdown = function(params) {
            document.getElementsByClassName("dropdown-toggle")[0].style.display = "none";
        }
        shinyjs.showDropdown = function(params) {
            document.getElementsByClassName("dropdown-toggle")[0].style.display = "block";
        }
        shinyjs.hideQCPlot = function(params) {
            if (document.getElementsByClassName("shiny-plot-output").length != 0)
                if (document.getElementsByClassName("shiny-plot-output")[0].getElementsByTagName("img").length != 0)
                    document.getElementsByClassName("shiny-plot-output")[0].getElementsByTagName("img")[0].style.display = "none";
        }
        shinyjs.showQCPlot = function(params) {
            if (document.getElementsByClassName("shiny-plot-output").length != 0)
                if (document.getElementsByClassName("shiny-plot-output")[0].getElementsByTagName("img").length != 0)
                    document.getElementsByClassName("shiny-plot-output")[0].getElementsByTagName("img")[0].style.display = "block";
        }
    '
    
enableBookmarking("server")

    dbHeader <- dashboardHeader(titleWidth = 250,
        dropdownMenu(type = "notifications", 
            badgeStatus = "primary", icon = shiny::icon("cog"),
            messageItem("Sign Out", "",
                                        icon = shiny::icon("sign-out")),
            messageItem("Refresh", "", 
                                        icon = shiny::icon("refresh"))
            ))
   dbHeader$children[[2]]$children <- tags$a(style='color: white;',
                                            id="top_logo" , "DEBrowser")
    addResourcePath(prefix = "www", directoryPath = system.file("extdata",
        "www", package = "debrowser"))
    if(!file.exists("shiny_saves/startup.rds")){
        startup_obj <- list()
        startup_obj$bookmark_counter <- 3
        startup_obj$startup_bookmark <- ""
        dir.create("shiny_saves")
        saveRDS(startup_obj, "shiny_saves/startup.rds")
    }        
        
    startup <- readRDS("shiny_saves/startup.rds")
    if (startup[['bookmark_counter']] == 0){
        debrowser <- (fluidPage(
            tags$script('Shiny.addCustomMessageHandler("testmessage",
                function(message) {
                    window.location.href = new_url;
                }
        );') ))
    }

    else{
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
            conditionalPanel(condition = "!output.user_name",
                googleAuthR::googleAuthUI("initial_google_button")),
            conditionalPanel(condition = "output.user_name",
                uiOutput("loading"),
                sidebarMenu(id="DataPrep",
                    menuItem("Upload", tabName = "Upload"),
                    menuItem("Filter", tabName = "Filter"),
                    menuItem("BatchEffect", tabName = "BatchEffect")
                ),
                conditionalPanel(condition = "(output.dataready)",
                    uiOutput("leftMenu")),
                conditionalPanel(condition = "(output.dataready)",
                    uiOutput("downloadSection")),
                conditionalPanel(condition = "(output.dataready)",
                    uiOutput('cutoffSelection')),
                    debrowser::bookmarkUI("bm")
            ), p("Logged in as: ", textOutput("user_name"))
        ),
    dashboardBody(
        conditionalPanel(condition = "output.user_name",
        mainPanel(
            width = 12,
            tags$head(
                tags$style(type = "text/css",
                        "#methodtabs.nav-tabs {font-size: 14px} ")),
                tabsetPanel(id = "methodtabs", type = "tabs",
                    tabPanel(title = "Data Prep", value = "panel0", id="panel0",
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
                             )),
                    tabPanel(title = "Main Plots", value = "panel1", id="panel1",
                            uiOutput("mainmsgs"),
                            conditionalPanel(condition = "input.demo || output.dataready", uiOutput("mainpanel"))),
                    tabPanel(title = "QC Plots", value = "panel2", id="panel2",
                            uiOutput("qcpanel")),
                    tabPanel(title = "GO Term", value = "panel3", id="panel3",
                            uiOutput("gopanel")),
                    tabPanel(title = "Tables", value = "panel4", id="panel4",
                            dataTableOutput("tables")))
        ),
        extendShinyjs(text = getUrlJSCode)
        )))
    )
    )
    }
    debrowser
}
