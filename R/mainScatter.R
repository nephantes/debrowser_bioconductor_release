#' mainScatter
#'
#' Creates the main scatter plot to be displayed within the main
#' panel.
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param lb, the linked brush
#' @param data_tooltip, toolstip specific to this plot
#' @param x, the name of the x coordinate
#' @param y, the name of the y coordinate
#' @param domains, the domains to be colored 
#' @param colors, colors for each domain
#' @return volcano plot
#'
#' @examples
#'     x <- mainScatter()
#'
#' @export
#'
mainScatter <- function(dat = NULL, lb = NULL,
    data_tooltip = NULL, x = NULL, y = NULL, 
    domains = NULL, colors = NULL) {
    if ( is.null(dat) ) return(NULL)

    dat %>% ggvis(~x, ~y) %>%
        layer_points( size := ~Size, size.hover := 200,
            fillOpacity := 0.2, fillOpacity.hover := 0.7,
            fill.brush := "red", opacity := 0.8, 
            stroke = ~Legend, key := ~ID) %>%
        lb$input() %>%
        add_tooltip(data_tooltip, "hover") %>%
        add_legend("stroke", title = "Plot",
            values = c(domains) ) %>%
            scale_nominal("stroke", domain = c(domains),
                range = c(colors) ) %>%
        hide_legend("fill") %>%
        set_options(width = "auto", height = 350, resizable=FALSE) %>%
        add_axis("x", title = x) %>%
        add_axis("y", title = y)
}

#' scatterZoom
#'
#' Displays the zoomed in version of the plot to be viewed within
#' the main panel.
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param data_tooltip, toolstip specific to this plot
#' @param x, the name of the x coordinate
#' @param y, the name of the y coordinate
#' @param domains, the domains to be colored 
#' @param colors, colors for each domain
#' @return zoomed scatter plot
#'
#' @examples
#'     x <- scatterZoom()
#'
#' @export
#'
scatterZoom <- function(dat = NULL, data_tooltip = NULL,
    x = NULL, y = NULL, domains = NULL, colors = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>% ggvis(~x, ~y) %>%
        layer_points(size := ~Size, size.hover := 200,
            stroke = ~Legend, key := ~ID, fill = ~padj) %>%
        add_tooltip(data_tooltip, "hover") %>%
        scale_nominal("stroke",  domain = c(domains),
            range = c(colors)) %>%
        add_legend("stroke", title = "Scatter Plot",
            values = c(domains)) %>%
        hide_legend("fill") %>%
        set_options(width = "auto", height = 350, resizable=FALSE) %>%
        add_axis("x", title = x) %>%
        add_axis("y", title = y)
}
