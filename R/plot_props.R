#' add_title_pos
#'
#' Adds a title with extra axis to ggvis plot and sets the positions
#'
#' @param vis, a ggvis plot
#' @param ..., any additional arguments
#' @param title for the plot
#' @param align position of the title c('left','right')
#' @param angle of the labels in x axis
#' @param dx, relative x position of the labels in the x axis
#' @param dy, relative y position of the labels in the x axis
#' @return deseq2 results 
#'
#' @export
#'
#' @examples
#'     require(ggvis)
#'     mtcars %>%
#'     ggvis(x=~cyl, y=~wt, fill=~mpg) %>%
#'     group_by(mpg) %>%
#'     layer_bars() %>%
#'     add_title_pos(title = "title", angle=310, dy=0, dx=0) %>%
#'     set_options(width = 400, height = 350)
#'
add_title_pos <- function(vis, ..., title = "Plot Title", 
    align = "left", angle = 0, dx = 0, dy = 0) {
        add_axis(vis, "x", title_offset=(-1*dy + 10), properties =
            axis_props(labels = list(angle = angle, align = align,
                fontSize = 10, dy = dx, dx = dy))) %>% 
            add_axis("x", orient = "top", ticks = 0, title = title,
                properties = axis_props(axis = list(stroke = "blank"),
                labels = list(fill = "blank", fontSize = 0),
                title = list(fontSize = 16)), ...)
}

#' getToolTipText
#'
#' Prepares tooltiptext for the second scatter plot in the plots page 
#'
#' @param dat, data need to have following columns; padj, average,
#'     cond1 and cond2 values, log10padj, foldChange 
#' @return tooltip text
#'
#' @export
#'
#' @examples
#' x <- getToolTipText()
#'
getToolTipText <- function(dat=NULL){
    if (is.null(dat)) return(NULL)

    paste("<b>", dat$ID, "</b><br>",
        "x=", round(dat$x, digits = 2), " ",
        "y=", round(dat$y, digits = 2),
        "<br>", "padj=", format.pval(dat[, "padj"]), " ",
        "-log10padj=", round(dat[, "log10padj"], digits = 2),
        "<br>", "log2FoldChange=", round(dat[, "log2FoldChange"],
            digits = 2), " ",
        "foldChange=", round(dat[, "foldChange"], digits = 2),
            "<br>", sep = " ")
}

#' getHoverPlots
#'
#' Prepares the plots going to be shown when a gene hovered
#' in the main plots 
#'
#' @param bardata, barplot data
#' @param genename, gene name in the barplots
#'
#' @export
#'
#' @examples
#'     getHoverPlots()
#'
getHoverPlots <- function(bardata=NULL, genename=NULL){
    if (is.null(bardata)) return(NULL)
    colnames(bardata) <- c("libs", "count", "conds")

    ypos <- -5 * max(nchar(as.vector(bardata$libs)))
    bardata$count <- as.numeric(as.character(bardata$count))

    dat <- rbind(bardata[bardata$conds == levels(bardata$conds)[1], ],
                 bardata[bardata$conds == levels(bardata$conds)[2], ])    
    title3 <- paste(genename, " variation")
    title4 <- paste(genename, " conditions")

    vis3 <- dat %>%
        ggvis(x = ~libs, y = ~count,
        fill = ~conds) %>%
    group_by(conds) %>% layer_bars() %>%
    add_title_pos(title = title3, angle = 310,
                    dy = ypos, dx = 0) %>%
        scale_ordinal('x', domain=dat$libs) %>%
        set_options(width = "auto", height = 350, resizable=FALSE)
    vis3 %>% bind_shiny("plot3")

    ypos <- -5 * max(nchar(as.vector(bardata$conds)))
    vis4 <- bardata %>%
        ggvis(x = ~conds, y = ~count,
            fill = ~conds) %>%
        group_by(conds) %>% layer_boxplots() %>%
        add_title_pos(title = title4, align = "middle", angle = 310,
                      dy = ypos, dx = 0) %>%
        set_options(width = "auto", height = 350, resizable=FALSE)
    vis4 %>% bind_shiny("plot4")
}




