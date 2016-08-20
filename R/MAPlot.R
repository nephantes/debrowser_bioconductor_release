#' MAPlot
#'
#' Prepares MA plot to be used within the main plot panel.
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param lb, the linked brush
#' @param data_tooltip, toolstip specific to this plot
#' @param domains, the domains to be colored 
#' @param colors, colors for each domain
#' @return MA plot
#'
#' @examples
#'     x <- MAPlot()
#'
#' @export
#'
MAPlot <- function(dat = NULL, lb = NULL, data_tooltip = NULL,
                domains = NULL, colors = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>%
    ggvis(~A, ~M) %>%
    layer_points( size := ~Size, size.hover := 200, fillOpacity := 0.2,
        fillOpacity.hover := 0.7, fill.brush := "red",
        opacity := 0.8, stroke = ~Legend, key :=  ~ID) %>%
        lb$input() %>% add_tooltip(data_tooltip, "hover") %>%
        add_legend("stroke", title = "Plot",
            values = c(domains) ) %>%
        scale_nominal("stroke", domain = c(domains),
            range = c(colors) ) %>%
        set_options(width = "auto", height = 350, resizable=FALSE)
}

#' MAZoom
#'
#' Prepares the zoomed in version of the MA plot to be used within
#' the main panel.
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param data_tooltip, toolstip specific to this plot
#' @param domains, the domains to be colored 
#' @param colors, colors for each domain
#' @return zoomed MA plot
#'
#' @examples
#'     x <- MAZoom()
#'
#' @export
#'
MAZoom <- function(dat = NULL, data_tooltip = NULL,
                domains = NULL, colors = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>% ggvis(~A, ~M) %>%
        layer_points(size := ~Size, size.hover := 200,
            stroke = ~Legend, key := ~ID, fill = ~padj) %>%
        add_tooltip(data_tooltip, "hover") %>%
        add_legend("stroke", title = "Plot",
            values = c(domains) ) %>%
        scale_nominal("stroke", domain = c(domains),
            range = c(colors) ) %>%
        hide_legend("fill") %>%
        set_options(width = "auto", height = 350, resizable=FALSE) 
}
