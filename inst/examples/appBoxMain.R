library(debrowser)

options(warn =-1)

header <- dashboardHeader(
    title = "DEBrowser Box Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DEAnlysis",
    menuItem("BoxMain", tabName = "BoxMain"),
    textInput("genename", "Gene/Region Name", value = "Foxa3" ),
    plotSizeMarginsUI("boxmain", h=400)
))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="BoxMain", 
                fluidRow(
                    column(12,
                           getBoxMainPlotUI("boxmain")))
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    observe({
        if (!is.null(input$genename))
            callModule(debrowserboxmainplot, "boxmain", demodata, 
                   metadatatable$sample,  
                   metadatatable$treatment, input$genename)
    })
    
}

shinyApp(ui, server)