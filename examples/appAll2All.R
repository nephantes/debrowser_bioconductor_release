library(shiny)
library(shinydashboard)
library(GGally)
library(plotly)
library(shinyjs)
source("../R/plotSize.R")
source("../R/funcs.R")
source("../R/all2all.R")

options(warn =-1)

header <- dashboardHeader(
  title = "DEBrowser All2All Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DEAnlysis",
      menuItem("All2All", tabName = "All2All"),
      getAll2AllMenuUI("all2all"),
      plotSizeMarginsUI("all2all", h=800, w=800)
))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="All2All", 
        fluidRow(
            column(12,
            getAll2AllPlotUI("all2all")))
    )
))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
    observe({
        callModule(debrowserall2all, "all2all", demodata)
    })
}

shinyApp(ui, server)