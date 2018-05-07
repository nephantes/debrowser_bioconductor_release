library(debrowser)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
source("../R/downloadData.R")
source("../R/plotSize.R")
source("../R/funcs.R")
source("../R/histogram.R")
source("../R/lowcountfilter.R")

header <- dashboardHeader(
    title = "DEBrowser Filter"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
       menuItem("Filter", tabName = "Filter")))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="Filter", dataLCFUI("lcf"),
                column(4,
                       verbatimTextOutput("filtertable")
                )
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    ldata <- reactiveValues(count=NULL, meta=NULL)
    ldata$count <- demodata
    ldata$meta <- metadatatable
    data <- callModule(debrowserlowcountfilter, "lcf", ldata)
    observe({
        output$filtertable <- renderPrint({
            head( data$filter()$count )
        })
    })
}

shinyApp(ui, server)