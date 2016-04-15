#' Prepares volcano plot
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param lb, the linked brush
#' @param data_tooltip, toolstip specific to this plot
#' @param domains, the domains to be colored 
#' @param colors, colors for each domain
#' @return volcano plot
#'
#' @examples
#'     x <- volcanoPlot()
#'
#' @export
volcanoPlot <- function(dat = NULL, lb = NULL, data_tooltip = NULL,
        domains = NULL, colors = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>% ggvis(x = ~log2FoldChange, y = ~log10padj) %>%
        layer_points(size := ~Size, size.hover := 200,
            fillOpacity := 0.2, fillOpacity.hover := 0.7,
            fill.brush := "red", opacity := 0.8,
            stroke = ~Legend, key := ~ID) %>%
        lb$input() %>%
        add_tooltip(data_tooltip, "hover") %>%
        add_legend("stroke", title = "Plot",
            values = c(domains) ) %>%
        scale_nominal("stroke", domain = c(domains),
            range = c(colors) ) %>%
        set_options(width = 400, height = 350)
}

#' Prepares volcano plot
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param data_tooltip, toolstip specific to this plot
#' @param domains, the domains to be colored 
#' @param colors, colors for each domain
#' @return zoomed volcano plot
#'
#' @examples
#'     x <- volcanoZoom()
#'
#' @export

volcanoZoom <- function(dat = NULL, data_tooltip = NULL,
    domains = NULL, colors = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>%
        ggvis(~log2FoldChange, ~log10padj) %>%
        layer_points(size := ~Size, size.hover := 200,
            stroke = ~Legend,  key := ~ID, fill = ~padj) %>%
        add_tooltip(data_tooltip, "hover") %>%
        add_legend("stroke", title = "Plot",
            values = c(domains) ) %>%
        scale_nominal("stroke", domain = c(domains),
            range = c(colors) ) %>%
        hide_legend("fill") %>%
        set_options(width = 400, height = 350, duration = 0)
}
