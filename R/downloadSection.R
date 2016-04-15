#' getDownloadSection, download section button and dataset selection
#'     box in the menu
#' @param flag, to show the download selection
#' @param type, main vs. QC section
#'
#' @note \code{getDownloadSection}
#' @return the panel for download section in the menu;
#'
#' @examples
#'     x<- getDownloadSection()
#'
#' @export
#'
getDownloadSection <- function(flag = FALSE, type = "main") {
    a <- NULL
    if (flag){
        if (type == "main")
            choices <- c("up+down", "up", "down", "selected",
                "comparisons", "alldetected", "geneset", "most-varied")
        else 
            choices <- c("alldetected", "geneset", "most-varied")
        a <- list(selectInput("dataset", "Choose a dataset:",
            choices = choices), 
            downloadButton("downloadData", "Download Data"),
            conditionalPanel(condition = "input.dataset=='most-varied'",
            textInput("topn", "top-n", value = "500" ), 
            textInput("mincount", "total min count", value = "10" )),
            conditionalPanel(condition = "input.dataset=='geneset'",
            textareaInput("genesetarea","Gene Set", 
                          "Fgf21", rows = 5, cols = 35),
            helpText("Regular expressions can be used<br>
                     Ex: ^Al => Al.., Al$ => ...al")
            ))
    }
    a
}
