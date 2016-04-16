#' shinyUI to be able to run interactively
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
    shinyUI(fluidPage(
        shinyjs::useShinyjs(),
        uiOutput("logo"),
        uiOutput("programtitle"),
        textOutput("text"),
        sidebarLayout(
            sidebarPanel(
            uiOutput("loading"),
            width = 2,
            uiOutput("initialmenu"),
            conditionalPanel(condition = "(input.goButton & output.dataready)",
                uiOutput('cutoffSelection')),
            uiOutput("downloadSection"),
            uiOutput("leftMenu")
        ),
        mainPanel(
            tags$head(
                tags$style(type = "text/css",
                    "#methodtabs.nav-tabs {font-size: 14px} ")),
            tabsetPanel(id = "methodtabs", type = "tabs",
                tabPanel(title = "Data Prep", value = "panel0", id="panel0",
                    uiOutput("preppanel")),
                tabPanel(title = "Main Plots", value = "panel1", id="panel1",
                    uiOutput("mainmsgs"),
                conditionalPanel(condition = "input.demo || output.dataready",
                    uiOutput("mainpanel"))),
                tabPanel(title = "QC Plots", value = "panel2", id="panel2",
                    uiOutput("qcpanel")),
                tabPanel(title = "GO Term", value = "panel3", id="panel3",
                    uiOutput("gopanel")),
                tabPanel(title = "All Detected", value = "panel4", id="panel4",
                    DT::dataTableOutput("table")),
                tabPanel(title = "Up", value = "panel5", id="panel5",
                    DT::dataTableOutput("up")),
                tabPanel(title = "Down", value = "panel6", id="panel6",
                    DT::dataTableOutput("down")),
                tabPanel(title = "Selected", value = "panel7", id="panel7",
                    DT::dataTableOutput("selected")),
                tabPanel(title = "Gene Set", value = "panel8", id="panel8",
                    DT::dataTableOutput("geneset")),
                tabPanel(title = "Most Varied", value = "panel9", id="panel9",
                    DT::dataTableOutput("mostvaried")),
                tabPanel(title = "Comparisons", value = "panel10", id="panel10",
                    DT::dataTableOutput("mergedcomp"))
            )
        )
    )
))
}
