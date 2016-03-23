#' getDownloadSection, download section button and dataset selection
#'     box in the menu
#'
#' @note \code{getDownloadSection}
#' @return the panel for download section in the menu;
#'
#' @examples
#'     x<- getDownloadSection()
#'
#' @export
#'
getDownloadSection <- function() {
    a <- list(selectInput("dataset", "Choose a dataset:",
                choices = c("up+down", "up", "down", "selected",
                "alldetected")), downloadButton("downloadData", "Download"))
}
