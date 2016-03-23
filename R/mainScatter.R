#' Main scatter plot
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param lb, the linked brush
#' @param data_tooltip, toolstip specific to this plot
#' @return volcano plot
#'
#' @examples
#'     x <- mainScatter()
#'
#' @export
#'

mainScatter <- function(dat = NULL, lb = NULL, data_tooltip = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>% ggvis(~Cond1, ~Cond2) %>%
        layer_points( size := 40, size.hover := 200,
            fillOpacity := 0.2, fillOpacity.hover := 0.7,
            fill.brush := "red", opacity := 0.8,
            stroke = ~Legend, key := ~ID) %>%
        lb$input() %>%
        add_tooltip(data_tooltip, "hover") %>%
        add_legend("stroke", title = "Plot",
            values = c("NS", "Up", "Down")) %>%
            scale_nominal("stroke", domain = c("NS", "Up", "Down"),
                range = c("#aaa", "green", "orange")) %>%
        set_options(width = 400, height = 350)
}

#' scatter zoom
#'
#' @param dat, dataframe that has log2FoldChange and log10padj values
#' @param data_tooltip, toolstip specific to this plot
#' @return zoomed scatter plot
#'
#' @examples
#'     x <- scatterZoom()
#'
#' @export
#'
scatterZoom <- function(dat = NULL, data_tooltip = NULL) {
    if ( is.null(dat) ) return(NULL)
    dat %>% ggvis(~Cond1, ~Cond2) %>%
        layer_points(size := 40, size.hover := 200,
            stroke = ~Legend, key := ~ID, fill = ~padj) %>%
        add_tooltip(data_tooltip, "hover") %>%
        scale_nominal("stroke",  domain = c("NS", "Up", "Down"),
            range = c("#aaa", "green", "orange")) %>%
        add_legend("stroke", title = "Scatter Plot",
            values = c("NS", "Up", "Down")) %>%
        hide_legend("fill") %>%
        set_options(width = 400, height = 350, duration = 0)
}
