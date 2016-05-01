#' getGoPanel
#'
#' Creates go term analysis panel within the shiny
#' display.
#' 
#' @param flag, flag to show the element in the ui
#' @note \code{getGoPanel}
#' @return the panel for go term analysis;
#'
#' @examples  
#'     x <- getGoPanel()
#'
#' @export
#' 
getGoPanel <- function(flag = FALSE){
    a <- NULL
    if (flag)
        a <- list(
            conditionalPanel(condition = "!input.startGO",
                helpText( "Please select parameters and press the 
                    submit button in the left menu
                    for the plots" )),
            tabsetPanel(id = "gotabs", type = "tabs",
                tabPanel(title = "Plot", value = "gopanel1", id="gopanel1",
                     column(12, wellPanel( plotOutput("GOPlots1")))),
                tabPanel(title = "Table", value = "gopanel2", id="gopanel2",
                     column(12, wellPanel( DT::dataTableOutput("gotable"))))
            ))
    a
}

#' getGOPlots
#'
#' Go term analysis panel.  Generates appropriate GO plot
#' based on user selection.
#'
#' @param dataset, the dataset used
#' @param input, input params
#' @param table, table flag to return dataset to show in a datatable
#' @note \code{getGOPlots}
#' @return the panel for go plots;
#'
#' @examples
#'     x<- getGOPlots(mtcars)
#' @export
#' 
getGOPlots <- function(dataset, input = NULL, table = FALSE){
    if (is.null(dataset) || is.null(input)) return(NULL)
    a <- NULL
    genelist <- getGeneList(rownames(dataset))
    if (input$goplot == "enrichGO"){
        res <- getEnrichGO(genelist, ont = input$ontology,
            pvalueCutoff = input$gopvalue)
        if (table)
            a<-res$table
        else{
        a <- res$p
        if (input$goextplot == "Dotplot")
            a <- dotplot(res$enrich_p, showCategory=30)
        }
    }
    else if (input$goplot == "enrichKEGG"){
        res <- getEnrichKEGG(genelist, pvalueCutoff=
            as.numeric(input$pvaluetxt))
        if (table)
            a<-res$table
        else{
            a <- res$p
            if (input$goextplot == "Dotplot")
                a <- dotplot(res$enrich_p, showCategory=30)
        }
    }
    else if (input$goplot == "compare"){
        cl <- clusterData(dataset)
        res <- compareClust(cl, fun=input$gofunc, input$ontology)
        if (table)
            a<-res$table
        else
            a<-res$p
    }
    else if (input$goplot == "disease"){
        res <- getEnrichDO(genelist, pvalueCutoff=as.numeric(input$pvaluetxt) )
        if (table)
            a<-res$table
        else{
            a <- res$p
            if (input$goextplot == "Dotplot")
                a <- dotplot(res$enrich_p, showCategory=30)
        }
    }
    a
}
