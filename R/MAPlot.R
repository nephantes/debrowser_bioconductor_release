#' Prepares volcano plot
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param lb, the linked brush
#' @param data_tooltip, toolstip specific to this plot
#' @return MA plot
#'
#' @examples
#'     x <- MAPlot()
#'
#' @export
#'

MAPlot <- function(dat = NULL, lb = NULL, data_tooltip = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>%
    ggvis(~A, ~M) %>%
    layer_points( size := 40, size.hover := 200, fillOpacity := 0.2,
        fillOpacity.hover := 0.7, fill.brush := "red",
        opacity := 0.8, stroke = ~Legend, key :=  ~ID) %>%
        lb$input() %>% add_tooltip(data_tooltip, "hover") %>%
        add_legend("stroke", title = "MA Plot",
        values = c("NS", "Up", "Down")) %>%
        scale_nominal("stroke", domain = c("NS", "Up", "Down"),
            range = c("#aaa", "green", "orange")) %>%
        set_options(width = 400, height = 350)
}

#' MA Zoom
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param data_tooltip, toolstip specific to this plot
#' @return zoomed MA plot
#'
#' @examples
#'     x <- MAZoom()
#'
#' @export
#'
MAZoom <- function(dat = NULL, data_tooltip = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>% ggvis(~A, ~M) %>%
        layer_points(size := 40, size.hover := 200,
            stroke = ~Legend, key := ~ID, fill = ~padj) %>%
        add_tooltip(data_tooltip, "hover") %>%
        scale_nominal("stroke", domain = c("NS", "Up", "Down"),
            range = c("#aaa", "green", "orange")) %>%
        add_legend("stroke", title = "MA Plot",
        values = c("NS", "Up", "Down")) %>% hide_legend("fill") %>%
        set_options(width = 400, height = 350, duration = 0)
}
