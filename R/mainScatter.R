#' getMainPlotUI
#'
#' main plot for volcano, scatter and maplot.  
#'
#' @note \code{getMainPlotUI}
#' @return the panel for main plots;
#'
#' @examples
#'     x <- getMainPlotUI()
#'
#' @export
#'
getMainPlotUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("mainplot"))
}

#' debrowsermainplot
#'
#' Module for a scatter, volcano and ma plots that are going to be used 
#' as a mainplot in debrowser
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     data <- generateTestData()
#'     input <- c()
#'     input$backperc <- 10
#'     input$mainplot <- "scatter"
#'     x <- debrowsermainplot(data = data)
#'
debrowsermainplot <- function(input, output, session, data = NULL) {
    if (is.null(data)) return(NULL)
    
    plotdata <-  reactive({ 
        plotData(data, input)
    })
    output$mainplot <- renderUI({
        list(fluidRow(
            column(12,
                   shinydashboard::box(
                       collapsible = TRUE, title = "Main Plots", status = "primary", 
                       solidHeader = TRUE,width = NULL,
                       draggable = TRUE, plotlyOutput(session$ns("main"), 
                                                      height=input$plotheight, width=input$plotwidth)
                   ))))
    })
    selectedPoint <- reactive({
        eventdata <- event_data("plotly_click", source = session$ns("source"))
        if (is.null(eventdata)){
            eventdata <- event_data("plotly_hover", source = session$ns("source"))
        }
        key <- ""
        if (!is.null(eventdata$key))
            key <- as.vector(unlist(eventdata$key))
        
        return(key)
    })
    
    getSelected  <- reactive({
        keys <- NULL
        selGeneList <- event_data("plotly_selected", source = session$ns("source"))
        if (is.null(selGeneList$key)) return (NULL)
        keys <- as.vector(unlist(selGeneList$key))
    })
    
    output$main <- renderPlotly({
        data <- plotdata()$data
        x <- plotdata()$x
        y <- plotdata()$y
        mainScatterNew(input, data, session$ns("source"), x, y)
    })
    
    list( shg = (selectedPoint), shgClicked=(selectedPoint), selGenes=(getSelected))
}


#' mainScatterNew
#'
#' Creates the main scatter, volcano or MA plot to be displayed within the main
#' panel.
#'
#' @param data, dataframe that has log2FoldChange and log10padj values
#' @param source, for event triggering to select genes
#' @param x, the name of the x coordinate
#' @param y, the name of the y coordinate
#' @return scatter, volcano or MA plot
#'
#' @examples
#'     
#'     x <- mainScatterNew(input, data, source, x, y)
#'
#' @export
#'
mainScatterNew <- function(input = NULL, data = NULL, source = NULL,
                        x = NULL, y = NULL) {
    if ( is.null(data) ) return(NULL)
    
    p <- plot_ly(source = source, data=data, x=~x, y=~y, key=~key,
                 color=~Legend, colors=c("grey", "blue", "red"), 
                 type="scatter", mode = "markers",
                 width=input$width - 100, height=input$height,
                 text=~paste("<b>", ID, "</b><br>",
                             "<br>", "padj=", format.pval(padj, digits = 2), " ",
                             "-log10padj=", round(log10padj, digits = 2),
                             "<br>", "log2FC=", round(log2FoldChange, digits = 2), " ",
                             "foldChange=", round(foldChange, digits = 2),
                             "<br>", sep = " ")) %>%
        layout(xaxis = list(title = x),
               yaxis = list(title = y)) %>% 
        plotly::layout(
            margin = list(l = input$left,
                          b = input$bottom,
                          t = input$top,
                          r = input$right
            ))
    p$elementId <- NULL
    
    return(p)
}

#' plotData
#'
#' prepare plot data for mainplots 
#'
#' @note \code{plotData}
#' @param data, data
#' @return prepdata
#' @examples
#'     x <- plotData(data)
#' @export
#'
plotData <- function(pdata, input){
    pdata$key <- pdata$ID
    data_rest <- pdata[ pdata$Legend!="NS",]
    data_NS <- pdata[ pdata$Legend=="NS",]
    backperc <- 10
    if (!is.null(input$backperc))  backperc <- input$backperc
    mainplot <- "scatter"
    if (!is.null(input$mainplot))  mainplot <- input$mainplot
    
    datapoints <- as.integer(nrow(data_NS) * backperc/ 100)
    if (nrow(data_NS) > datapoints){
        data_rand <- data_NS[sample(1:nrow(data_NS), datapoints,
                                    replace=FALSE),]
    }else{
        data_rand  <- data_NS
    }
    plot_init_data <- rbind(data_rand, data_rest)
    plot_init_data$Legend  <- factor(plot_init_data$Legend, 
                                     levels = unique(plot_init_data$Legend))
    
    plot_data <- plot_init_data
    if (mainplot == "volcano") {
        plot_data <- plot_init_data[which(!is.na(plot_init_data$log2FoldChange)
                                          & !is.na(plot_init_data$log10padj)
                                          & !is.na(plot_init_data$Legend)),]
        plot_data$x <- plot_data$log2FoldChange
        plot_data$y <- plot_data$log10padj
        x <- "log2FC"
        y <- "log10padj"
    } else if (mainplot == "scatter") {
        x <-  "x"
        y <-  "y"
    } else if (mainplot == "maplot") {
        plot_data$x <- (plot_init_data$x + plot_init_data$y) / 2
        plot_data$y <- plot_init_data$x - plot_init_data$y
        x <- "A"
        y <- "M"
    }
    list( data = (plot_data), x=(x), y=(y))
}

#' mainPlotControlsUI
#'
#' Generates the left menu to be used for main plots
#'
#' @note \code{mainPlotControlsUI}
#' @param id, module ID
#' @return mainPlotControls
#' @examples
#'     x <- mainPlotControlsUI("main")
#' @export
#'
mainPlotControlsUI <- function(id) {
    getMainPlotsLeftMenu(id)
}

#' getMainPlotsLeftMenu
#'
#' Generates the Main PLots Left menu to be displayed within the DEBrowser.
#'
#' @note \code{getMainPlotsLeftMenu}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getMainPlotsLeftMenu()
#' @export
#'
getMainPlotsLeftMenu <- function(id) {
    ns <- NS(id)
    list(shinydashboard::menuItem(" Plot Type",
                                  startExpanded=TRUE,
                                  radioButtons(ns("mainplot"), "Main Plots:",
                                               c(Scatter = "scatter", VolcanoPlot = "volcano",
                                                 MAPlot = "maplot"))
    ),
    shinydashboard::menuItem("Main Options",
                             sliderInput(ns("backperc"), "Background Data(%):",
                                         min=10, max=100, value=10, sep = "",
                                         animate = FALSE)))
}

#' generateTestData
#'
#' This generates a test data that is suitable to main plots in debrowser
#'
#' @return testData
#'
#' @examples
#'     x <- generateTestData(data)
#'
#' @export
#'
generateTestData <- function(dat) {
    
    ##################################################
    columns <- dat$columns
    conds <- dat$conds
    data <- dat$data
    deseqrun <- runDESeq(data, columns, conds)
    
    met <- as.data.frame(cbind(as.vector(conds), columns))
    colnames(met) <- c("conds", "columns")
    cols1 <- as.vector(met[met$conds==as.character(unique(met$conds)[1]), "columns"])
    cols2 <- as.vector(met[met$conds==as.character(unique(met$conds)[2]), "columns"])
    
    de_res <- data.frame(deseqrun)
    norm_data <- getNormalizedMatrix(data[, columns])
    rdata <- cbind(rownames(de_res), norm_data[rownames(de_res), columns],
                   log10(rowMeans(norm_data[rownames(de_res),cols1])
                         + 0.1), log10( rowMeans( norm_data[ rownames( de_res ), cols2])
                                        + 0.1), de_res[rownames(de_res),
                                                       c("padj", "log2FoldChange")], 2 ^ de_res[rownames(de_res),
                                                                                                "log2FoldChange"], -1 *
                       log10(de_res[rownames(de_res), "padj"]))
    colnames(rdata) <- c("ID", columns, "x", "y", "padj",
                         "log2FoldChange", "foldChange", "log10padj")
    rdata <- as.data.frame(rdata)
    rdata$padj[is.na(rdata$padj)] <- 1
    
    padj_cutoff <- 0.01
    foldChange_cutoff <- 2
    
    
    rdata$Legend <- "NS"
    rdata$Legend[rdata$log2FoldChange > log2(foldChange_cutoff) &
                     rdata$padj < padj_cutoff] <- "Up"
    rdata$Legend[rdata$log2FoldChange <= log2(1 / foldChange_cutoff) &
                     rdata$padj < padj_cutoff] <- "Down"
    
    dat <- rdata
    dat$M <- rdata$x - rdata$y
    dat$A <- (rdata$x + rdata$y) / 2
    dat
}

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
