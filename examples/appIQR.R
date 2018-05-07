library(shiny)
library(heatmaply)
library(shinyjs)
library(reshape2)
source("../R/plotSize.R")
source("../R/funcs.R")
source("../R/IQR.R")

header <- dashboardHeader(
  title = "DEBrowser IQR Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataAssessment",
    menuItem("IQR", tabName = "IQR"),
    textInput("maxCutoff", "Max Cutoff", value = "10" ),
    plotSizeMarginsUI("IQR", w=400, h=400),
    plotSizeMarginsUI("afterFiltering", w=400, h=400)
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName="IQR", 
    fluidRow(
        column(5,
               getIQRPlotUI("IQR")),
        column(5,
               getIQRPlotUI("afterFiltering"))
    ))
))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
  filtd <- reactive({
      # Filter out the rows that has maximum 100 reads in a sample
      subset(demodata, apply(demodata, 1, max, na.rm = TRUE)  >=  
                 as.numeric(input$maxCutoff))
  })
  observe({
      if(!is.null(filtd())){
          callModule(debrowserIQRplot, "IQR", demodata)
          callModule(debrowserIQRplot, "afterFiltering", filtd())
      }
  })
}

shinyApp(ui, server)