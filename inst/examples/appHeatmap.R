library(debrowser)
library(DESeq2)
library(heatmaply)
library(RColorBrewer)
library(gplots)
source("../../R/heatmap.R")
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
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    insulinSignalingGenes <- reactive({
        genes <- c("Prkar2a", "Tsc1", "Mapk8", "Sos1", "Pik3r1", "Srebf1",
                   "Insr", "Fasn", "Ppp1r3b", "Pik3r3", "Ptprf", "Pklr",
                   "Irs2", "Socs4", "Eif4ebp1", "Ppp1r3c", "Pygl", "Socs2",
                   "Cbl","Acaca", "Crkl")
        normDat <- getNormalizedMatrix(demodata, method = "MRN")
        normDat[genes, ]
    })
    selected <- reactiveVal()
    observe({
        withProgress(message = 'Creating plot', style = "notification", value = 0.1, {
            selected(callModule(debrowserheatmap, "heatmap", insulinSignalingGenes()))
        })
    })
    output$heatmap_hover <- renderPrint({
        if (!is.null(selected()) && !is.null(selected()$shgClicked()) && 
            selected()$shgClicked() != "")
            return(paste0("Clicked: ",selected()$shgClicked()))
        else
            return(paste0("Hovered:", selected()$shg()))
    })
    output$heatmap_selected <- renderPrint({
        if (!is.null(selected()))
            selected()$selGenes()
    })
}

shinyApp(ui, server)