#' getSamples
#'
#' Gathers the sample names to be used within DEBrowser.
#'
#' @param cnames, names of the  samples
#' @param index, starting column in a tab separated file
#' @return choices
#' @export
#'
#' @examples
#'     x <- getSamples()
#'
getSamples <- function (cnames = NULL, index = 1) { 
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

#' prepDEOutput
#'
#' Prepares the output data from DE analysis to be used within
#' DEBrowser
#'
#' @param data, loaded dataset
#' @param cols, columns
#' @param conds, conds
#' @param inputconds, inputconds
#' @param i, selected comparison number
#' @param input, input
#' @return data
#' @export
#'
#' @examples
#'     x <- prepDEOutput()
#'
prepDEOutput <- function(data = NULL, cols = NULL, 
    conds = NULL, inputconds=NULL, i=NULL, input = NULL) {
    if (is.null(data)) return (NULL)
    if (length(cols) != length(conds)) return(NULL)
    params <- inputconds$demethod_params[i]
    de_res <- runDE(data, cols, conds, params)
    de_res <- data.frame(de_res)
}


#' applyFilters
#'
#' Applies filters based on user selected parameters to be
#' displayed within the DEBrowser.
#'
#' @param filt_data, loaded dataset
#' @param cols, selected samples
#' @param conds, seleced conditions
#' @param input, input parameters
#' @return data
#' @export
#'
#' @examples
#'     x <- applyFilters()
#'
applyFilters <- function(filt_data = NULL, cols = NULL, conds=NULL,
    input = NULL){
    if (is.null(input$padjtxt) || is.null(input$foldChangetxt) 
        || is.null(filt_data)) return(NULL)
    compselect <- 1
    if (!is.null(input$compselect) ) 
        compselect <- as.integer(input$compselect)
    x <- paste0("Cond", 2*compselect - 1) 
    y <- paste0("Cond", 2*compselect)
    norm_data <- getNormalizedMatrix(filt_data[, cols], 
        input$norm_method)
    g <- data.frame(cbind(cols, conds))
    if (length(as.vector(g[g$conds == x, "cols"])) > 1 )
        filt_data$x <- log10(rowMeans(norm_data[, 
            as.vector(g[g$conds == x, "cols"])]) + 0.1)
    else
        filt_data$x <- log10(norm_data[, 
             as.vector(g[g$conds == x, "cols"])] + 0.1)
    if (length(as.vector(g[g$conds == y, "cols"])) > 1 )
        filt_data$y <- log10(rowMeans(norm_data[, 
             as.vector(g[g$conds == y, "cols"])]) + 0.1)
    else
        filt_data$y <- log10(norm_data[, 
             as.vector(g[g$conds == y, "cols"])] + 0.1)
    filt_data[,cols] <- norm_data
    
    padj_cutoff <- as.numeric(input$padjtxt)
    foldChange_cutoff <- as.numeric(input$foldChangetxt)
    m <- filt_data
    # Add column which says whether a gene significant or not
    m$Legend <- character(nrow(m))
    m$Size <- character(nrow(m))
    m[, "Size"] <- "40"
    m$Legend <- "NS"
    if (input$dataset == "up" || input$dataset == "up+down") 
        m$Legend[m$foldChange >= foldChange_cutoff &
               m$padj <= padj_cutoff] <- "Up"
    if (input$dataset == "down" || input$dataset == "up+down")
        m$Legend[m$foldChange <= (1 / foldChange_cutoff) &
               m$padj <= padj_cutoff] <- "Down"
    if (input$dataset == "most-varied" && !is.null(cols)) {
        most_varied <- getMostVariedList(m, cols, input)
        m[rownames(most_varied), c("Legend")] <- "MV"
    }
    if (input$dataset == "selected" &&
        !is.null(input$genenames)) {
        selectedGenes <- unlist(strsplit(input$genenames, ","))
        m[selectedGenes, c("Legend")] <- "GS"
    }
    if (!is.null(input$genesetarea) && input$genesetarea != ""
        && input$methodtabs == "panel1") {
        genelist <- getGeneSetData(m, c(input$genesetarea))
        m[rownames(genelist), "Legend"] <- "GS"
        m[rownames(genelist), "Size"] <- "100"
        m <- m[rev(order(m$Legend)),]
    }
    m
}
#' getSelectedDatasetInput
#'
#' Gathers the user selected dataset output to be displayed.
#'
#' @param rdata, filtered dataset
#' @param getSelected, selected data
#' @param getMostVaried, most varied data
#' @param mergedComparison, merged comparison data
#' @param explainedData, pca set
#' @param input, input parameters
#' @return data
#' @export
#'
#' @examples
#'     x <- getSelectedDatasetInput()
#'
getSelectedDatasetInput<-function(rdata = NULL, getSelected = NULL, 
    getMostVaried = NULL, mergedComparison = NULL, explainedData = NULL, 
    input = NULL) {
    if (is.null(rdata)) return (NULL)
    m <- rdata
    if (input$dataset == "up") {
        m <- getUp(rdata)
    } else if (input$dataset == "down") {
        m <- getDown(rdata)
    } else if (input$dataset == "up+down") {
        m <- getUpDown(rdata)
    } else if (input$dataset == "alldetected") {
        m <- rdata
    } else if (input$dataset == "selected") {
        m <- getSelected
    } else if (input$dataset == "most-varied") {
        m <- getMostVaried
    } else if (input$dataset == "comparisons") {
        m <- mergedComparison
    } else if (input$dataset == "searched") {
        m <- searched
    } else if (input$dataset == "pcaset") {
       m <- explainedData
    }
    m
}


#' getMostVariedList
#'
#' Calculates the most varied genes to be used for specific plots
#' within the DEBrowser.
#'
#' @param datavar, loaded dataset
#' @param cols, selected columns
#' @param input, input 
#' @return data
#' @export
#'
#' @examples
#'     x <- getMostVariedList()
#'
getMostVariedList <- function(datavar = NULL, cols = NULL, input = NULL){
    if (is.null(datavar)) return (NULL)
    topn <- as.integer(as.numeric(input$topn))
    datavar <- datavar[rowSums(datavar[,cols]) >
        as.integer(as.numeric(input$mincount)), cols]
    cv<-cbind(apply(datavar, 1, function(x) 

        (sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))), 1)
    colnames(cv)<-c("coeff", "a")
    cvsort<-cv[order(cv[,1],decreasing=TRUE),]
    topindex<-nrow(cvsort)
    if (topindex > topn) topindex <- topn
    cvsort_top <- head(cvsort, topindex)
    selected_var <- data.frame(datavar[rownames(cvsort_top),])
}


#' getSearchData
#'
#' search the geneset in the tables and return it
#'
#' @param dat, table data
#' @param input, input params
#' @return data
#' @export
#'
#' @examples
#'     x <- getSearchData()
#'
getSearchData <- function(dat = NULL, input = NULL)
{
  if (is.null(dat)) return(NULL)
  if (input$genesetarea != ""){
    dat <- getGeneSetData(dat, c(input$genesetarea))
  }
  dat
}

#' getGeneSetData
#'
#' Gathers the specified gene set list to be used within the
#' DEBrowser.
#'
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
    
    geneset1 <- unique(unlist(strsplit(geneset, split="[:;, \t\n\t]")))
    geneset2 <- geneset1[geneset1 != ""]
    if(length(geneset2) > 20)
        geneset2 <- paste0("^", geneset2, "$")
    
    dat1 <- as.data.frame(data)
    if(!("ID" %in% names(dat1)))
        dat2 <- addID(dat1)
    else
        dat2 <- dat1

    dat2$ID<-factor(as.character(dat2$ID))

    geneset4 <- unique(as.vector(unlist(lapply(toupper(geneset2), 
        function(x){ sapply(dat2[(grepl(x, toupper(dat2[,"ID"]))), "ID"], 
                            as.character) }))))
    retset <- data.frame(dat2[geneset4, ])
    retset
}

#' getUp
#' get up regulated data
#'
#' @param filt_data, filt_data
#' @return data
#' @export
#'
#' @examples
#'     x <- getUp()
#'
getUp <- function(filt_data = NULL){
    if(is.null(filt_data)) return(NULL)
    filt_data[
        filt_data[, "Legend"] == "Up" | 
        filt_data[, "Legend"] == "GS", ]
}
#' getDown
#' get down regulated data
#'
#' @param filt_data, filt_data
#' @return data
#' @export
#'
#' @examples
#'     x <- getDown()
#'
getDown <- function(filt_data = NULL){
    if(is.null(filt_data)) return(NULL)
    filt_data[
        filt_data[, "Legend"] == "Down"|
        filt_data[, "Legend"] == "GS", ]
}

#' getUpDown
#' get up+down regulated data
#'
#' @param filt_data, filt_data
#' @return data
#' @export
#'
#' @examples
#'     x <- getUpDown()
#'
getUpDown <- function(filt_data = NULL){
    if(is.null(filt_data)) return(NULL)
    filt_data[
        filt_data[, "Legend"] == "Up" | 
        filt_data[, "Legend"] == "Down"|
        filt_data[, "Legend"] == "GS", ]
}

#' getDataForTables
#' get data to fill up tables tab
#'

#' @param input, input parameters
#' @param init_data, initial dataset
#' @param filt_data, filt_data
#' @param selected, selected genes
#' @param getMostVaried, most varied genes
#' @param mergedComp, merged comparison set
#' @param explainedData, pca gene set
#' @return data
#' @export
#'
#' @examples
#'     x <- getDataForTables()
#'
getDataForTables <- function(input = NULL, init_data = NULL,
    filt_data = NULL, selected = NULL,
    getMostVaried = NULL,  mergedComp = NULL,
    explainedData = NULL){
    if (is.null(init_data )) return(NULL)
    pastr <- "padj"
    fcstr <- "foldChange"
    dat <- NULL
    if (input$dataset == "alldetected"){
        if (!is.null(init_data)){
            dat <- getSearchData(init_data, input)
        }
    }
    else if (input$dataset == "up+down"){
        if (!is.null(filt_data))
            dat <- getSearchData(getUpDown(filt_data), input)
    }
    else if (input$dataset == "up"){
        if (!is.null(filt_data))
            dat <- getSearchData(getUp(filt_data), input)
    }
    else if (input$dataset == "down"){
        if (!is.null(filt_data))
            dat <- getSearchData(getDown(filt_data), input)
    }
    else if (input$dataset == "selected"){
        dat <- getSearchData(selected$data$getSelected(), input)
    }
    else if (input$dataset == "pcaset"){
        dat <- getSearchData( explainedData, input )
    }
    else if (input$dataset == "most-varied"){
        dat <- getSearchData(getMostVaried, input)
    }
    else if (input$dataset == "comparisons"){
        if (is.null(mergedComp)) return(NULL)
        fcstr<-colnames(mergedComp)[grepl("foldChange", colnames(mergedComp))]
        pastr<-colnames(mergedComp)[grepl("padj", colnames(mergedComp))]
        dat <- getSearchData(mergedComp, input)
    }
    list(dat, pastr, fcstr)
}


#' getMergedComparison
#'
#' Gathers the merged comparison data to be used within the
#' DEBrowser.
#' @param Dataset, whole data
#' @param dc, data container
#' @param nc, the number of comparisons
#' @param input, input params
#' @return data
#' @export
#'
#' @examples
#'     x <- getMergedComparison()
#'
getMergedComparison <- function(Dataset = NULL, dc = NULL, nc = NULL, input = NULL){
    merged <- c()
    if (is.null(dc)) return (NULL)
    merged <- Dataset[,input$samples]

    for ( ni in seq(1:nc)) {
        tmp <- dc[[ni]]$init_data[,c("foldChange", "padj")]
        
        tt <- paste0("C", (2*ni-1),".vs.C",(2*ni))
        fctt <- paste0("foldChange.", tt)
        patt <-  paste0("padj.", tt)
        colnames(tmp) <- c(fctt,  patt)

        merged[,fctt] <- character(nrow(merged))
        merged[,patt] <- character(nrow(merged))
        merged[rownames(tmp),c(fctt, patt)] <- tmp[rownames(tmp),c(fctt, patt)]
        merged[rownames(tmp),patt] <- tmp[rownames(tmp),patt]
        merged[merged[,fctt]=="",fctt] <- 1 
        merged[merged[,patt]=="",patt] <- 1 
    }
    merged
}

#' applyFiltersToMergedComparison
#'
#' Gathers the merged comparison data to be used within the
#' DEBrowser.
#'
#' @param merged, merged data 
#' @param nc, the number of comparisons
#' @param input, input params
#' @return data
#' @export
#'
#' @examples
#'     x <- applyFiltersToMergedComparison()
#'
applyFiltersToMergedComparison <- function (merged = NULL, 
    nc = NULL, input = NULL)
{
    if (is.null(merged)) return (NULL)
    padj_cutoff <- as.numeric(input$padjtxt)
    foldChange_cutoff <- as.numeric(input$foldChangetxt)
    if (is.null(merged$Legend)){
        merged$Legend <- character(nrow(merged))
        merged$Legend <- "NS"
    }
    for ( ni in seq(1:nc)) {
        tt <- paste0("C", (2*ni-1),".vs.C",(2*ni))
        merged[which(as.numeric(merged[,c(paste0("foldChange.", tt))]) >= 
            foldChange_cutoff & as.numeric(merged[,c(paste0("padj.", tt))]) <= 
            padj_cutoff), "Legend"] <- "Sig"
        merged[which(as.numeric(merged[,c(paste0("foldChange.", tt))]) <= 
            1/foldChange_cutoff & as.numeric(merged[,c(paste0("padj.", tt))]) <= 
            padj_cutoff), "Legend"] <- "Sig"
    }
    merged 
}

#' removeCols
#'
#' remove unnecessary columns
#'
#' @param cols, columns that are going to be removed from data frame
#' @param dat, data
#' @return data
#' @export
#'
#' @examples
#'     x <- removeCols()
#'
removeCols <- function( cols = NULL, dat = NULL) {
    if (is.null(dat)) return (NULL)
    for (colnum in seq(1:length(cols))){
         if (cols[colnum] %in% colnames(dat) )
              dat[, cols[colnum]]<- NULL
    }
    dat
}