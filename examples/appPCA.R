library(shiny)
library(heatmaply)
library(shinyjs)
source("../R/plotSize.R")
source("../R/funcs.R")
source("../R/pca.R")

header <- dashboardHeader(
    title = "DEBrowser PCA Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataAssessment",
    menuItem("PCA", tabName = "PCA"),
    menuItem("PCA Options",
    pcaPlotControlsUI("pca")),
    plotSizeMarginsUI("pca", w=600, h=400, t=50, b=50, l=60, r=0)
    ))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="PCA", getPCAPlotUI("pca"),
                column(4,
                       verbatimTextOutput("pca_hover"),
                       verbatimTextOutput("pca_selected")
                )
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")


server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    selected <- callModule(debrowserpcaplot, "pca", demodata)
    
    #output$main_hover <- renderPrint({
    #    selected$shgClicked()
    #})

    #output$main_selected <- renderPrint({
    #     selected$selGenes()
    #})

}

shinyApp(ui, server)