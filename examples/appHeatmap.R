library(shiny)
library(shinydashboard)
library(heatmaply)
library(shinyjs)
library(gplots)
library(colourpicker)
source("../R/plotSize.R")
source("../R/heatmap.R")
options(warn=-1)
header <- dashboardHeader(
    title = "DEBrowser Heatmap"
)
sidebar <- dashboardSidebar(  getJSLine(), sidebarMenu(id="DataAssessment",
           menuItem("Heatmap", tabName = "Heatmap"),
           plotSizeMarginsUI("heatmap"),
           heatmapControlsUI("heatmap")))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="Heatmap",  getHeatmapUI("heatmap"),
                column(4,
                       verbatimTextOutput("heatmap_hover"),
                       verbatimTextOutput("heatmap_selected")
                )
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    #filtd <-
        # Filter out the rows that has maximum 100 reads in a sample
    #    subset(a, apply(a, 1, max, na.rm = TRUE)  >=  10)
    withProgress(message = 'Creating plot', style = "notification", value = 0.1, {
        selected <- callModule(debrowserheatmap, "heatmap", filtd)
    })
    
    output$heatmap_hover <- renderPrint({
        if (selected$shgClicked() != "")
            return(paste0("Clicked: ",selected$shgClicked()))
        else
            return(paste0("Hovered:", selected$shg()))
    })
    output$heatmap_selected <- renderPrint({
         selected$selGenes()
    })
}

shinyApp(ui, server)