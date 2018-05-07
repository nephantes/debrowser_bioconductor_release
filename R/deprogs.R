#' debrowserdeanalysis
#'
#' Module to perform and visualize DE results.
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @return DE panel 
#' @export
#'
#' @examples
#'     x <- debrowserdeanalysis(data = data)
#'
debrowserdeanalysis <- function(input, output, session, data = NULL, columns = NULL, conds = NULL, params = NULL) {
    deres <- reactive({
        runDE(data, columns, conds, params)
    })
    prepDat <- reactive({
        applyFilters(addDataCols(data, deres(), columns, conds), input)
    })
    observe({
        setFilterParams(session, isolate(input))
        dat <-  prepDat()[prepDat()$Legend == input$legendradio,]
        getTableDetails(output, session, "DEResults", dat, modal=FALSE)
    })
    list(dat = prepDat)
}
#' getDEResultsUI
#' Creates a panel to visualize DE results
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- getDEResultsUI("batcheffect")
#'
#' @export
#'
getDEResultsUI<- function (id) {
    ns <- NS(id)
    list(
        fluidRow(
            shinydashboard::box(title = "DE Results",
            solidHeader = T, status = "info",  width = 12, 
            fluidRow(
                column(12,
                uiOutput(ns("DEResults"))
                ))
            )
        )
        )
}

#' cutOffSelectionUI
#'
#' Gathers the cut off selection for DE analysis
#'
#' @param nc, total number of comparisons
#' @note \code{cutOffSelectionUI}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- cutOffSelectionUI()
#' @export
#'
cutOffSelectionUI <- function(id){
    ns <- NS(id)
    list(
        tags$head(tags$script(HTML(logSliderJScode(ns("padj"))))),
            getLegendRadio(id),
            sliderInput(ns("padj"), "padj value cut off",
                min=0, max=10, value=6, sep = "",
                animate = FALSE),
        textInput(ns("padjtxt"), "or padj", value = "0.01" ),
        sliderInput(ns("foldChange"), "Fold Change cut off",
            1, 20, 2, step = 0.1),
        textInput(ns("foldChangetxt"), "or foldChange", value = "2" )
    )
}

#' setFilterParams
#'
#' It sets the filter parameters 
#'
#' @param session, session variable
#' @param input, input parameters
#' @export
#'
#' @examples
#'     x <- setFilterParams()
#'
setFilterParams <- function(session = NULL, input = NULL) {
    if (!is.null(input$padj)){
        if (input$padj %% 2)
            valpadj = (10 ^ (-1*as.integer(
                (10-input$padj)/2 )) ) /2
        else
            valpadj = (10 ^ (-1*(10-input$padj)/2))
        if(input$padj == 0) valpadj = 0
        updateTextInput(session, "padjtxt",
            value = valpadj ) 
    }
    if (!is.null(input$gopvalue)){
        if (input$gopvalue%%2)
            gopval = (10 ^ (-1*as.integer(
                (10-input$gopvalue)/2 )) ) /2
        else
            gopval = (10 ^ (-1*(10-input$gopvalue)/2))
        if(input$gopvalue==0) gopval = 0
        updateTextInput(session, "pvaluetxt",
            value = gopval ) 
    }
    if (!is.null(input$foldChange)){
        valpadjfoldChange = input$foldChange
        updateTextInput(session, "foldChangetxt",
            value = valpadjfoldChange)
    }
}

#' applyFilters
#'
#' Apply filters based on foldChange cutoff and padj value.
#' This function adds a "Legend" column with "Up", "Down" or
#' "NS" values for visualization.
#'
#' @param data, loaded dataset
#' @param input, input parameters
#' @return data
#' @export
#'
#' @examples
#'     x <- applyFilters()
#'
applyFilters <- function(data, input) {
    if (is.null(data)) return(NULL)
    padj_cutoff <- as.numeric(input$padjtxt)
    foldChange_cutoff <- as.numeric(input$foldChangetxt)
    m <- data
    if (!("Legend" %in% names(m))) {
        m$Legend <- character(nrow(m))
        m$Legend <- "NS"
    }
    m$Legend[m$foldChange >= foldChange_cutoff &
        m$padj <= padj_cutoff] <- "Up"
    m$Legend[m$foldChange <= (1 / foldChange_cutoff) &
        m$padj <= padj_cutoff] <- "Down"
    return(m)
}

    
#' runDE
#'
#' Run DE algorithms on the selected parameters.  Output is
#' to be used for the interactive display.
#'
#' @param data, A matrix that includes all the expression raw counts,
#'     rownames has to be the gene, isoform or region names/IDs
#' @param columns, is a vector that includes the columns that are going
#'     to be analyzed. These columns has to match with the given data.
#' @param conds, experimental conditions. The order has to match
#'     with the column order
#' @param params, all params for the DE methods
#' @return de results
#'
#' @export
#'
#' @examples
#'     x <- runDE()
#'
runDE <- function(data = NULL, columns = NULL, conds = NULL, params = NULL) {
    if (is.null(data)) return(NULL)
    de_res <- NULL

    if (params[1] == "DESeq2")     
        de_res <- runDESeq2(data, columns, conds, params)
    else if (params[1]  == "EdgeR")     
        de_res <- runEdgeR(data, columns, conds, params)
    else if (params[1] == "Limma")
        de_res <- runLimma(data, columns, conds, params)
    data.frame(de_res)
}

#' runDESeq2
#'
#' Run DESeq2 algorithm on the selected conditions.  Output is
#' to be used for the interactive display.
#'
#' @param data, A matrix that includes all the expression raw counts,
#'     rownames has to be the gene, isoform or region names/IDs
#' @param columns, is a vector that includes the columns that are going
#'     to be analyzed. These columns has to match with the given data.
#' @param conds, experimental conditions. The order has to match
#'     with the column order
#' @param params, fitType: either "parametric", "local", or "mean" for the type 
#'     of fitting of dispersions to the mean intensity. 
#'     See estimateDispersions for description.
#'  betaPrior: whether or not to put a zero-mean normal prior
#'     on the non-intercept coefficients See nbinomWaldTest for 
#'     description of the calculation of the beta prior. By default, 
#'     the beta prior is used only for the Wald test, but can also be 
#'     specified for the likelihood ratio test.
#' testType: either "Wald" or "LRT", which will then use either 
#'     Wald significance tests (defined by nbinomWaldTest), or the 
#'     likelihood ratio test on the difference in deviance between a 
#'     full and reduced model formula (defined by nbinomLRT)
#' rowsum.filter: regions/genes/isoforms with total count 
#'      (across all samples) below this value will be filtered out
#' @return deseq2 results
#'
#' @export
#'
#' @examples
#'     x <- runDESeq2()
#'
runDESeq2 <- function(data = NULL, columns = NULL, conds = NULL, params) {
    fitType <- if (!is.null(params[2])) params[2]
    betaPrior <-  if (!is.null(params[3])) params[3]
    testType <- if (!is.null(params[4])) params[4]
    rowsum.filter <-  if (!is.null(params[5])) as.integer(params[5])

    if (is.null(data)) return (NULL)
    data <- data[, columns]

    data[, columns] <- apply(data[, columns], 2,
        function(x) as.integer(x))

    coldata <- prepGroup(conds, columns)
    # Filtering non expressed genes
    filtd <- data
    if (is.numeric(rowsum.filter) && !is.na(rowsum.filter))
        filtd <- subset(data, rowSums(data) > rowsum.filter)
    
    # DESeq data structure is going to be prepared
    dds <- DESeqDataSetFromMatrix(countData = as.matrix(filtd),
        colData = coldata, design = ~group)
    # Running DESeq
    if (testType == "LRT")
        dds <- DESeq(dds, fitType = fitType, betaPrior = as.logical(betaPrior), test=testType, reduced= ~ 1)
    else
        dds <- DESeq(dds, fitType = fitType, betaPrior = as.logical(betaPrior), test=testType)

    res <- results(dds)
    return(res)
}

#' runEdgeR
#'
#' Run EdgeR algorithm on the selected conditions.  Output is
#' to be used for the interactive display.
#'
#' @param data, A matrix that includes all the expression raw counts,
#'     rownames has to be the gene, isoform or region names/IDs
#' @param columns, is a vector that includes the columns that are going
#'     to be analyzed. These columns has to match with the given data.
#' @param conds, experimental conditions. The order has to match
#'     with the column order
#' @param params, normfact: Calculate normalization factors to scale the raw 
#'     library sizes. Values can be "TMM","RLE","upperquartile","none".
#' dispersion: either a numeric vector of dispersions or a character 
#'     string indicating that dispersions should be taken from the data 
#'     object. If a numeric vector, then can be either of length one or 
#'     of length equal to the number of genes. Allowable character 
#'     values are "common", "trended", "tagwise" or "auto". 
#'     Default behavior ("auto" is to use most complex dispersions 
#'     found in data object.
#' testType: exactTest or glmLRT. exactTest: Computes p-values for differential 
#'     abundance for each gene between two digital libraries, conditioning 
#'     on the total count for each gene. The counts in each group as a 
#'     proportion of the whole are assumed to follow a binomial distribution. 
#'     glmLRT: Fit a negative binomial generalized log-linear model to the read 
#'     counts for each gene. Conduct genewise statistical tests for a given 
#'     coefficient or coefficient contrast.
#' rowsum.filter: regions/genes/isoforms with total count 
#'      (across all samples) below this value will be filtered out
#' @return edgeR results
#'
#' @export
#'
#' @examples
#'     x <- runEdgeR()
#'
runEdgeR<- function(data = NULL, columns = NULL, conds = NULL, params = NULL){
    normfact <- if (!is.null(params[2])) params[2]
    dispersion <- if (!is.null(params[3])) params[3]
    testType <- if (!is.null(params[4])) params[4]
    rowsum.filter <- if (!is.null(params[5])) params[5]
    if (is.null(data)) return (NULL)
    data <- data[, columns]
    data[, columns] <- apply(data[, columns], 2,
        function(x) as.integer(x))
    dispersion <- as.numeric(dispersion)
    conds <- factor(conds)
    filtd <- data
    if (!is.null(rowsum.filter))
        filtd <- subset(data, rowSums(data) > rowsum.filter)
    
    d<- edgeR::DGEList(counts = filtd, group=conds)
    d <- edgeR::calcNormFactors(d, method = normfact)
    # If dispersion is 0, it will estimate the dispersions.
    de.com <- c() 
    if (testType == "exactTest"){
        if (dispersion == 0){
            d <- edgeR::estimateCommonDisp(d)
            de.com <- edgeR::exactTest(d)
        }else{
            de.com <- edgeR::exactTest(d, dispersion=dispersion)
        }
    }else if (testType == "glmLRT"){
        cnum = summary(conds)[levels(conds)[1]]
        tnum = summary(conds)[levels(conds)[2]]
        des <- c(rep(1, cnum),rep(2, tnum))
        design <- model.matrix(~des)
        if (dispersion == 0){
            d <- edgeR::estimateCommonDisp(d)
            fit <- edgeR::glmFit(d, design)
        }else{
            fit <- edgeR::glmFit(d, design, dispersion=dispersion)
        }   
        de.com <- edgeR::glmLRT(fit)
    }

    options(digits=4)

    padj<- p.adjust(de.com$table$PValue, method="BH")
    res <-data.frame(cbind(de.com$table$logFC/log(2),de.com$table$PValue, padj))
    colnames(res) <- c("log2FoldChange", "pvalue", "padj")
    rownames(res) <- rownames(filtd)
    return(res)
}

#' runLimma
#'
#' Run Limma algorithm on the selected conditions.  Output is
#' to be used for the interactive display.
#'
#' @param data, A matrix that includes all the expression raw counts,
#'     rownames has to be the gene, isoform or region names/IDs
#' @param columns, is a vector that includes the columns that are going
#'     to be analyzed. These columns has to match with the given data.
#' @param conds, experimental conditions. The order has to match
#'     with the column order
#' @param params, normfact: Calculate normalization factors to scale the raw 
#'     library sizes. Values can be "TMM","RLE","upperquartile","none".
#' fitType, fitting method; "ls" for least squares or "robust" 
#'     for robust regression
#' normBet: Normalizes expression intensities so that the 
#'     intensities or log-ratios have similar distributions across a set of arrays.
#' rowsum.filter: regions/genes/isoforms with total count 
#'     (across all samples) below this value will be filtered out
#' @return Limma results
#'
#' @export
#'
#' @examples
#'     x <- runLimma()
#'
runLimma<- function(data = NULL, columns = NULL, conds = NULL){
    normfact = if (!is.null(params[2])) params[2]
    fitType = if (!is.null(params[3])) params[3]
    normBet = if (!is.null(params[4])) params[4]
    rowsum.filter <-if (!is.null(params[5])) params[5]
    if (is.null(data)) return (NULL)
    data <- data[, columns]
    data[, columns] <- apply(data[, columns], 2,
        function(x) as.integer(x))
    conds <- factor(conds)
 
    cnum = summary(conds)[levels(conds)[1]]
    tnum = summary(conds)[levels(conds)[2]]
    filtd <- data
    if (!is.null(rowsum.filter))
         filtd <- as.matrix(subset(data, rowSums(data) > rowsum.filter))
    
    des <- factor(c(rep(levels(conds)[1], cnum),rep(levels(conds)[2], tnum)))
    names(filtd) <- des
    design <- cbind(Grp1=1,Grp2vs1=des)
   
    dge <- DGEList(counts=filtd, group = des)
    
    dge <- calcNormFactors(dge, method=normfact, samples=columns)
    
    v <- voom(dge, design=design, normalize.method = normBet, plot=FALSE)
    
    fit <- lmFit(v, design=design)
    fit <- eBayes(fit)
    
    options(digits=4)
    tab <- topTable(fit,coef=2, number=dim(fit)[1],genelist=fit$genes$NAME)
    res <-data.frame(cbind(tab$logFC, tab$P.Value, tab$adj.P.Val))
    
    colnames(res) <- c("log2FoldChange", "pvalue", "padj")
    rownames(res) <- rownames(tab)
    return(res)
}

#' prepGroup
#'
#' prepare group table
#'
#' @param cols, columns
#' @param conds, inputconds
#' @return data
#' @export
#'
#' @examples
#'     x <- prepGroup()
#'
prepGroup <- function(conds = NULL, cols = NULL) {
    coldata <- data.frame(cbind(cols, conds))
    coldata$conds <- factor(coldata$conds)
    colnames(coldata) <- c("libname", "group")
    coldata
}

#' addDataCols
#'
#' add aditional data columns to de results
#'
#' @param data, loaded dataset
#' @param de_res, de results
#' @param cols, columns
#' @param conds, inputconds
#' @return data
#' @export
#'
#' @examples
#'     x <- addDataCols()
#'
addDataCols <- function(data = NULL, de_res = NULL, cols = NULL, conds = NULL) {
    if (is.null(data) || is.null(de_res)) return (NULL)
    norm_data <- data[, cols]
    
    coldata <- prepGroup(conds, cols)
    
    mean_cond_first <- getMean(norm_data, as.vector(coldata[coldata$group==levels(coldata$group)[1], "libname"]))
    mean_cond_second <- getMean(norm_data, as.vector(coldata[coldata$group==levels(coldata$group)[2], "libname"]))
    
    m <- cbind(rownames(de_res), norm_data[rownames(de_res), cols],
               log10(unlist(mean_cond_second) + 1),
               log10(unlist(mean_cond_first) + 1),
               de_res[rownames(de_res),
                      c("padj", "log2FoldChange", "pvalue")], 
               2 ^ de_res[rownames(de_res),
                          "log2FoldChange"],
               -1 * log10(de_res[rownames(de_res), "padj"]))
    colnames(m) <- c("ID", cols, "x", "y",
                     "padj", "log2FoldChange", "pvalue",
                     "foldChange", "log10padj")
    m <- as.data.frame(m)
    m$padj[is.na(m[paste0("padj")])] <- 1
    m$pvalue[is.na(m[paste0("pvalue")])] <- 1
    m
}

#' getMean
#'
#' Gathers the mean for selected condition.
#'
#' @param data, dataset
#' @param selcols, input cols
#' @return data
#' @export
#'
#' @examples
#'     x <- getMean()
#'
getMean<-function(data = NULL, selcols=NULL) {
    if (is.null(data)) return (NULL)
    mean_cond<-NULL
    if (length(selcols) > 1)
        mean_cond <-list(rowMeans( data[, selcols]))
    else
        mean_cond <-list(norm_data[selcols])
    mean_cond
}


#' getLegendRadio
#'
#' Radio buttons for the types in the legend
#'
#' @note \code{getLegendRadio}
#' @return radio control
#'
#' @examples
#'    
#'     x <- getLegendRadio()
#'
#' @export
#'
getLegendRadio <- function(id) {
    ns <- NS(id)
    types <- c("Up", "Down", "NS")
    radioButtons(inputId=ns("legendradio"), 
                 label="Data Type:",
                 choices=types
                 )
}


