library(debrowser)
library(Harman)
library(sva)
library(shiny)
library(shinyBS)
library(reshape2)
library(shinydashboard)
library(plotly)
library(DESeq2)
source("../R/plotSize.R")
source("../R/funcs.R")
source("../R/batcheffect.R")
source("../R/IQR.R")
source("../R/pca.R")
source("../R/density.R")
source("../R/deprogs.R")

header <- dashboardHeader(
    title = "DEBrowser Batch Effect"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
       menuItem("BatchEffect", tabName = "BatchEffect")))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="BatchEffect", batchEffectUI("batcheffect"),
                column(4,
                       verbatimTextOutput("batcheffecttable")
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
    data <- callModule(debrowserbatcheffect, "batcheffect", ldata)
    observe({
        output$batcheffecttable <- renderPrint({
            head( data$BatchEffect()$count )
        })
    })
}

shinyApp(ui, server)