#' getGoPanel, go term analysis panel
#'
#' @note \code{getGoPanel}
#' @return the panel for go term analysis;
#'
#' @examples
#'     x <- getGoPanel()
#'
#' @export
#'
getGoPanel <- function(){
    a <- list(
        column(3,
            sliderInput("gopvalue", "p-value cutoff",
                0, 1, 0.01, step = 0.001)),
            conditionalPanel (( condition <- "input.goplot=='enrichGO' ||
                input.goplot=='compare'"),
                    column( 3,
                        selectInput("ontology", "Choose an ontology:",
                            choices =  c( "CC", "MF", "BP"))
            )),
            conditionalPanel (( condition <- "input.goplot!='compare'"),
                    column( 3,
                        selectInput("goextplot", "Plot Type:",
                            choices =  c("Summary", "Dotplot"))
            )),
            conditionalPanel (( condition <- "input.goplot=='compare'"),
                    column( 3,
                        selectInput("gofunc", "Plot Function:",
                            choices =  c( "groupGO", "enrichGO", "enrichKEGG"))
            )),
            column( 3,
                actionButton("startGO", "Submit"),
                downloadButton("downloadGOPlot", "Download")),
            column(12,
                wellPanel( plotOutput("GOPlots1"))) )
    a
}

#' getGOPlots, go term analysis panel
#'
#' @param dataset, the dataset used
#' @param inputGO, input Gene Ontology
#' @param genelist, list of genes
#' @note \code{getGOPlots}
#' @return the panel for go plots;
#'
#' @examples
#'     x<- getGOPlots(mtcars)
#'
#' @export
#'
getGOPlots <- function(dataset, inputGO = NULL, genelist = list(1)){
    a <- NULL
    if (!is.null(dataset) && !is.null(inputGO)){
        if (inputGO$goplot == "enrichGO"){
            res <- getEnrichGO(genelist, ont = inputGO$ontology,
                pvalueCutoff = inputGO$gopvalue)
            a <- res$p
            if (inputGO$goextplot == "Dotplot"){
                a <- dotplot(res$enrich_p, showCategory = 30)
            }
            else if (inputGO$goextplot == "enrichMap"){
                a <- enrichMap(res$enrich_p, vertex.label.cex = 1.2,
                    layout = igraph::layout.kamada.kawai)
            }
        }
        else if (inputGO$goplot == "enrichKEGG"){
            res <- getEnrichKEGG(genelist, pvalueCutoff = inputGO$gopvalue)
            a <- res$p
            if (inputGO$goextplot == "Dotplot"){
                a <- dotplot(res$enrich_p, showCategory = 30)
            }
        }
        else if (inputGO$goplot == "compare"){
            cl <- clusterData(dataset)
            a <- compareClust(cl, fun = inputGO$gofunc, inputGO$ontology)
        }
        else if (inputGO$goplot == "disease"){
            res <- getEnrichDO(genelist, pvalueCutoff = inputGO$gopvalue )
            a <- res$p
            if (inputGO$goextplot == "Dotplot"){
                a <- dotplot(res$enrich_p, showCategory = 30)
            }
        }
    }
    a
}
