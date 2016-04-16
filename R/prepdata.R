#' getSamples
#' @param cnames, names of the  samples
#' @param index, starting column in a tab separated file
#' @return choices
#' @export
#'
#' @examples
#'     x <- getSamples()
#'
getSamples <- function (cnames = NULL, index = 2) { 
    m <- NULL
    if (!is.null(cnames)) {
        cn <- cnames[index:length(cnames)]
        m <- as.list(NULL)
        for (i in seq(cn)) {
            m[i] <- cn[i]
        }
    }
    m
}

#' prepDataContainer
#' @param data, loaded dataset
#' @param counter, the number of comparisons
#' @param input, input parameters
#' @param session, session var
#' @return data
#' @export
#'
#' @examples
#'     x <- prepDataContainer()
#'
prepDataContainer <- function(data = NULL, counter=NULL, 
    input = NULL, session=NULL) {
    if (is.null(input$goButton)) return(NULL)
    if (input$goButton[1]==0) return(NULL)
    inputconds <- reactiveValues(fittype=NULL, conds = list())
    inputconds <- eventReactive(input$goButton, {
    m <- c()
    updateTabsetPanel(session, "methodtabs", selected = "panel1")
    hide(selector = "#methodtabs li a[data-value=panel0]")
    shiny::validate(need(input$condition1, "Condition1 has to be selected"),
        need(input$condition2, "Condition2 has to be selected"))
    m$conds <- list()
    for (cnt in seq(1:(2*counter)))
    {
        m$conds[cnt] <- list(input[[paste0("condition",cnt)]])
    }
    m$fittype <- input$fittype
    shinyjs::disable("goButton")
    m
    })
    if (is.null(input$condition1)) return(NULL)
    dclist<-list()
    for (i in seq(1:counter))
    {
        conds <- c(rep(paste0("Cond", 2*i-1), 
        length(inputconds()$conds[[2*i-1]])), 
        rep(paste0("Cond", 2*i), length(inputconds()$conds[[2*i]])))
        cols <- c(paste(inputconds()$conds[[2*i-1]]), 
        paste(inputconds()$conds[[2*i]]))
        m<-prepDESeqOutput(data, cols, conds, inputconds(), i)
        m<-list(conds = conds, cols = cols, init_data=m)
        dclist[[i]] <- m
    }
    togglePanels(1, c(1:10), session)
    return(dclist)
}

#' getMean
#' @param norm_data, loaded dataset
#' @param de_res, de results
#' @param inputconds, input parameters
#' @param colnum, colnum
#' @return data
#' @export
#'
#' @examples
#'     x <- getMean()
#'
getMean<-function(norm_data = NULL, de_res = NULL, 
    inputconds = NULL, colnum = NULL) {
    if (is.null(norm_data)) return (NULL)
    mean_cond<-NULL
    if (length(inputconds$conds[[colnum]]) > 1)
        mean_cond <-list(rowMeans( norm_data[ rownames( de_res ),
            paste( inputconds$conds[[colnum]] )] ))
    else
        mean_cond <-list(norm_data[ rownames( de_res ),
            paste( inputconds$conds[[colnum]] )])
    mean_cond
}

#' prepDESeqOutput
#' @param data, loaded dataset
#' @param cols, columns
#' @param conds, conds
#' @param inputconds, inputconds
#' @param i, selected comparison number
#' @return data
#' @export
#'
#' @examples
#'     x <- prepDESeqOutput()
#'
prepDESeqOutput <- function(data = NULL, cols = NULL, 
    conds = NULL, inputconds=NULL, i=NULL) {
    if (is.null(data)) return (NULL)
    if (length(cols) == length(conds))
        de_res <- runDESeq(data, cols, conds, inputconds$fittype,
            non_expressed_cutoff = 10)
    de_res <- data.frame(de_res)
    norm_data <- getNormalizedMatrix(data[, cols])
    mean_cond <- c()
    mean_cond_first <- getMean(norm_data, de_res, 
        inputconds, 2*i-1)
    mean_cond_second <- getMean(norm_data, de_res, 
        inputconds, 2*i)
    m <- cbind(rownames(de_res), norm_data[rownames(de_res), cols],
        log10(unlist(mean_cond_first) + 0.1),
        log10(unlist(mean_cond_second) + 0.1),
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
    m
}

#' applyFilters
#' @param filt_data, loaded dataset
#' @param cols, selected samples
#' @param input, input parameters
#' @return data
#' @export
#'
#' @examples
#'     x <- applyFilters()
#'
applyFilters <- function(filt_data = NULL, cols = NULL, 
    input = NULL){
    if (is.null(input$padjtxt) || is.null(input$foldChangetxt) 
        || is.null(filt_data)) return(NULL)

    padj_cutoff <- as.numeric(input$padjtxt)
    foldChange_cutoff <- as.numeric(input$foldChangetxt)
    m <- filt_data
    # Add column which says whether a gene significant or not
    m$Legend <- character(nrow(m))
    m$Size <- character(nrow(m))
    m[, "Size"] <- "40"
    m$Legend[abs(m$log2FoldChange) <= log2(foldChange_cutoff)] <- "NS"
    m$Legend[is.null(m$log10padj) ||  m$Legend==""] <- "NS"
    if (input$dataset == "most-varied" && !is.null(cols)) {
        m[, "Legend"] <- "NA"
        most_varied <- getMostVariedList(m, cols, input$topn, input$mincount)
        m[rownames(most_varied), c("Legend")] <- "MV"
    }
    else if (input$dataset == "geneset") {
        genelist <- getGeneSetData(m, c(input$genesetarea))
        m[, "Legend"] <- "NA"
        m[rownames(genelist), "Legend"] <- "GS"
        m[rownames(genelist), "Size"] <- "100"
        m <- m[rev(order(m$Legend)),]
    }
    else{
        m$Legend[m$log2FoldChange > log2(foldChange_cutoff) &
            m$padj < padj_cutoff] <- "Up"
        m$Legend[m$log2FoldChange < log2(1 / foldChange_cutoff) &
            m$padj < padj_cutoff] <- "Down"
    }
    m
}

#' getSelectedDatasetInput
#' @param rdata, filtered dataset
#' @param getSelected, selected data
#' @param getMostVaried, most varied data
#' @param getGeneSet, given gene set
#' @param getMergedComparison, merged comparison data
#' @param input, input parameters
#' @return data
#' @export
#'
#' @examples
#'     x <- getSelectedDatasetInput()
#'
getSelectedDatasetInput<-function(rdata = NULL, getSelected = NULL, 
    getMostVaried = NULL, getGeneSet = NULL, getMergedComparison = NULL, 
    input = NULL) {
    if (is.null(rdata)) return (NULL)
    m <- rdata
    if (input$dataset == "up") {
        m <- rdata[rdata[, "Legend"] == "Up", ]
    } else if (input$dataset == "down") {
        m <- rdata[rdata[, "Legend"] == "Down", ]
    } else if (input$dataset == "up+down") {
        m <- rdata[which(rdata[, "Legend"] == "Down" |
            rdata[, "Legend"] == "Up"), ]
    } else if (input$dataset == "alldetected") {
        m <- rdata
    } else if (input$dataset == "selected") {
        m <- getSelected
    } else if (input$dataset == "geneset") {
      m <- getGeneSet
    } else if (input$dataset == "most-varied") {
        m <- getMostVaried
    } else if (input$dataset == "comparisons") {
        m <- getMergedComparison
    }
    m
}
#' prepDataForQC
#' @param dataset, loaded dataset
#' @return data
#' @export
#'
#' @examples
#'     x <- prepDataForQC()
#'
prepDataForQC<-function(dataset = NULL){
    if (is.null(dataset)) return (NULL)
    columns <-colnames(dataset)
    dataset <- data.frame(dataset[,columns])
    dataset[, columns] <- apply(dataset[, columns], 2,
        function(x) as.integer(x))
    dataset1 <- rowSums(dataset[,1:ncol(dataset)])
    filtd <- subset(dataset, rowSums(dataset[,1:ncol(dataset)]) > 10)

    norm_data <- getNormalizedMatrix(filtd)
    return(norm_data)
}

#' getMostVariedList
#' @param datavar, loaded dataset
#' @param cols, selected columns
#' @param topn, most varied records
#' @param mincount, total min read count for selected samples
#' @return data
#' @export
#'
#' @examples
#'     x <- getMostVariedList()
#'
getMostVariedList <- function(datavar = NULL, cols = NULL,
    topn = 500, mincount = 10){
    if (is.null(datavar)) return (NULL)
    topn <- as.integer(as.numeric(topn))
    mincount <- as.integer(as.numeric(mincount))
    norm_data_var <- getNormalizedMatrix(
        datavar[rowSums(datavar[,cols])>mincount,cols])  
    cv<-cbind(apply(norm_data_var, 1, function(x) 
        (sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))), 1)
    colnames(cv)<-c("coeff", "a")
    cvsort<-cv[order(cv[,1],decreasing=TRUE),]
    topindex<-nrow(cvsort)
    if (topindex > topn) topindex <- topn
    cvsort_top <- head(cvsort, topindex)
    selected_var <- data.frame(datavar[rownames(cvsort_top),])
}
#' getGeneSetData
#' @param data, loaded dataset
#' @param geneset, given gene set
#' @return data
#' @export
#'
#' @examples
#'     x <- getGeneSetData()
#'
getGeneSetData <- function(data = NULL, geneset = NULL) {
    if (is.null(data)) return (NULL)
    geneset <- unique(unlist(strsplit(geneset, split="[:;, \t\n\t]")))
    geneset <- geneset[geneset != ""]
    dat1 <- data.frame(data)
    if(!("ID" %in% names(dat1)))
        dat1 <- addID(data)

    rownames(dat1) <- toupper(dat1$ID)
    geneset
    #geneset<-toupper(geneset[(toupper(geneset) %in% toupper(data$ID))])
    geneset <- unique(as.vector(unlist(lapply(toupper(geneset), 
        function(x){ dat1$ID[(grepl(x, toupper(dat1$ID)))] }))))
    retset <- data[geneset, ]
    retset
}
#' addID
#' @param data, loaded dataset
#' @return data
#' @export
#'
#' @examples
#'     x <- addID()
#'
addID <- function(data = NULL) {
    if (is.null(data)) return (NULL)
    dat1 <- data.frame(data)
    dat1 <- cbind(rownames(data), data)
    colnames(dat1) <- c("ID", colnames(data))
    dat1
}
#' getMergedComparison
#' @param dc, data container
#' @param nc, the number of comparisons
#' @return data
#' @export
#'
#' @examples
#'     x <- getMergedComparison()
#'
getMergedComparison <- function(dc = NULL, nc = NULL){
    merged <- c()
    if (is.null(dc)) return (NULL)
    for ( ni in seq(1:nc)) {
        tmp <- dc[[ni]]$init_data[,c("foldChange", "pvalue", "padj")]
        tt <- paste0("C", (2*ni-1),".vs.C",(2*ni))
        colnames(tmp) <- c(paste0("foldChange.", tt),  
            paste0("pvalue", tt), paste0("padj", tt))
        if (ni==1){
            merged <- tmp
        }
        else{
            merged <- merge(merged, tmp, by="row.names")
            row.names(merged) <- merged$Row.names
            merged[,c("Row.names")] <- NULL
        }
    }
    merged
}
