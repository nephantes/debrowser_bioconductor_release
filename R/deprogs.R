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
#' @param pars, all params for the de methods
#' @return de results
#'
#' @export
#'
#' @examples
#'     x <- runDE()
#'
runDE <- function(data = NULL, columns = NULL, conds = NULL, pars = NULL) {
    if (is.null(data)) return(NULL)
    de_res <- NULL
    pars <- unlist(strsplit(pars, ","))
    if (pars[1] == "DESeq2")     
        de_res <- runDESeq2(data, columns, conds, pars[2], pars[3], pars[4], pars[5])
    else if (pars[1]  == "EdgeR")     
        de_res <- runEdgeR(data, columns, conds, pars[2], pars[3], pars[4], pars[5])
    else if (pars[1] == "Limma")
        de_res <- runLimma(data, columns, conds, pars[2], pars[3], pars[4], pars[5])
    de_res
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
#' @param fitType, either "parametric", "local", or "mean" for the type 
#'     of fitting of dispersions to the mean intensity. 
#'     See estimateDispersions for description.
#' @param betaPrior, whether or not to put a zero-mean normal prior
#'     on the non-intercept coefficients See nbinomWaldTest for 
#'     description of the calculation of the beta prior. By default, 
#'     the beta prior is used only for the Wald test, but can also be 
#'     specified for the likelihood ratio test.
#' @param testType, either "Wald" or "LRT", which will then use either 
#'     Wald significance tests (defined by nbinomWaldTest), or the 
#'     likelihood ratio test on the difference in deviance between a 
#'     full and reduced model formula (defined by nbinomLRT)
#' @param rowsum.filter, regions/genes/isoforms with total count 
#'      (across all samples) below this value will be filtered out
#' @return deseq2 results
#'
#' @export
#'
#' @examples
#'     x <- runDESeq2()
#'
runDESeq2 <- function(data = NULL, columns = NULL, conds = NULL,
    fitType = c("parametric", "local", "mean"),
    betaPrior = 0,
    testType = c("Wald", "LRT"),
    rowsum.filter = 10) {
    if (is.null(data)) return (NULL)
    data <- data[, columns]

    data[, columns] <- apply(data[, columns], 2,
        function(x) as.integer(x))

    conds <- factor(conds)

    coldata <- data.frame(colnames(data))
    coldata <- cbind(coldata, conds)
    colnames(coldata) <- c("libname", "group")
    # Filtering non expressed genes
    filtd <- subset(data, rowSums(data) > rowsum.filter)

    # DESeq data structure is going to be prepared
    dds <- DESeqDataSetFromMatrix(countData = as.matrix(filtd),
        colData = coldata, design = ~group)

    # Running DESeq
    dds <- DESeq(dds, fitType = fitType)
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
#' @param normfact, Calculate normalization factors to scale the raw 
#'     library sizes. Values can be "TMM","RLE","upperquartile","none".
#' @param dispersion, either a numeric vector of dispersions or a character 
#'     string indicating that dispersions should be taken from the data 
#'     object. If a numeric vector, then can be either of length one or 
#'     of length equal to the number of genes. Allowable character 
#'     values are "common", "trended", "tagwise" or "auto". 
#'     Default behavior ("auto" is to use most complex dispersions 
#'     found in data object.
#' @param testType, exactTest or glmLRT. exactTest: Computes p-values for differential 
#'     abundance for each gene between two digital libraries, conditioning 
#'     on the total count for each gene. The counts in each group as a 
#'     proportion of the whole are assumed to follow a binomial distribution. 
#'     glmLRT: Fit a negative binomial generalized log-linear model to the read 
#'     counts for each gene. Conduct genewise statistical tests for a given 
#'     coefficient or coefficient contrast.
#' @param rowsum.filter, regions/genes/isoforms with total count 
#'      (across all samples) below this value will be filtered out
#' @return edgeR results
#'
#' @export
#'
#' @examples
#'     x <- runEdgeR()
#'
runEdgeR<- function(data = NULL, columns = NULL, conds = NULL,
    normfact = c("TMM","RLE","upperquartile","none"),
    dispersion = 0,
    testType = c("glmLRT", "exactTest"),
    rowsum.filter = 10) {
    if (is.null(data)) return (NULL)
    data <- data[, columns]
    data[, columns] <- apply(data[, columns], 2,
                             function(x) as.integer(x))
    dispersion <- as.numeric(dispersion)
    conds <- factor(conds)
    
    filtd <- subset(data, rowSums(data) > rowsum.filter)
    
    d<- DGEList(counts = filtd, group=conds)
    d <- calcNormFactors(d, method = normfact)
    # If dispersion is 0, it will estimate the dispersions.
    de.com <- c() 
    if (testType == "exactTest"){
        if (dispersion == 0){
            d <- estimateCommonDisp(d)
            de.com <- exactTest(d)
        }else{
            de.com <- exactTest(d, dispersion=dispersion)
        }
    }else if (testType == "glmLRT"){
        cnum = summary(conds)[levels(conds)[1]]
        tnum = summary(conds)[levels(conds)[2]]
        des <- c(rep(1, cnum),rep(2, tnum))
        design <- model.matrix(~des)
        if (dispersion == 0){
            d <- estimateCommonDisp(d)
            fit <- glmFit(d, design)
        }else{
            fit <- glmFit(d, design, dispersion=dispersion)
        }   
        de.com <- glmLRT(fit)
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
#' @param normfact, Calculate normalization factors to scale the raw 
#'     library sizes. Values can be "TMM","RLE","upperquartile","none".
#' @param fitType, fitting method; "ls" for least squares or "robust" 
#'     for robust regression
#' @param normBet, Normalizes expression intensities so that the 
#'     intensities or log-ratios have similar distributions across a set of arrays.
#' @param rowsum.filter, regions/genes/isoforms with total count 
#'     (across all samples) below this value will be filtered out
#' @return Limma results
#'
#' @export
#'
#' @examples
#'     x <- runLimma()
#'
runLimma<- function(data = NULL, columns = NULL, conds = NULL,
    normfact = c("none", "TMM", "RLE", "upperquartile"),
    fitType = c("ls", "robust"),
    normBet = c("none", "scale", "quantile", "cyclicloess",
        "Aquantile", "Gquantile", "Rquantile","Tquantile"),
    rowsum.filter = 10) {
    if (is.null(data)) return (NULL)
    data <- data[, columns]
    data[, columns] <- apply(data[, columns], 2,
        function(x) as.integer(x))
    conds <- factor(conds)
    
    cnum = summary(conds)[levels(conds)[1]]
    tnum = summary(conds)[levels(conds)[2]]
    
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

#' runBayseq
#'
#' Run Bayseq algorithm on the selected conditions.  Output is
#' to be used for the interactive display.
#'
#' @param data, A matrix that includes all the expression raw counts,
#'     rownames has to be the gene, isoform or region names/IDs
#' @param columns, is a vector that includes the columns that are going
#'     to be analyzed. These columns has to match with the given data.
#' @param conds, experimental conditions. The order has to match
#'     with the column order
#' @param rowsum.filter, regions/genes/isoforms with total count 
#'     (across all samples) below this value will be filtered out
#' @return BaySeq results
#'
#' @export
#'
#' @examples
#'     x <- runBayseq()
#'
runBayseq<- function(data = NULL, columns = NULL, conds=NULL,
                     rowsum.filter = 10) {
    if ( is.null(data) ) return(NULL)
    data <- data[, columns]
    data[, columns] <- apply(data[, columns], 2,
                             function(x) as.integer(x))
    conds <- factor(conds)
    
    cnum = summary(conds)[levels(conds)[1]]
    tnum = summary(conds)[levels(conds)[2]]
    cname <- rownames(filtd)
    filtd <- as.matrix(subset(data, rowSums(data) > rowsum.filter))
    des <- c(rep(1, cnum),rep(2, tnum))

    CD <- new("countData", data = filtd,
               replicates = conds,
               groups = list(NDE = c(rep(1, cnum+tnum)),
               DE = des))
    CD@annotation <- as.data.frame(cname)
    cl <- NULL
    libsizes(CD) <- getLibsizes(CD)
    densityFunction(CD) <- nbinomDensity
    CD <- getPriors(CD, cl = cl)
    CD <- getLikelihoods(CD, cl = cl)
    CDP.NBML <- getPriors.NB(CD, samplesize = 1000, estimation = "QL", cl = cl)
    CDPost.NBML <- getLikelihoods.NB(CDP.NBML, pET = 'BIC', cl = cl)
    CDPost.NBML@estProps
    
    options(digits=4)
    tab <- topTable(fit,coef=2,number=dim(fit)[1],genelist=fit$genes$NAME)
    res <-data.frame(cbind(tab$logFC/log(2), tab$P.Value, tab$adj.P.Val))
    
    colnames(res) <- c("log2FoldChange", "pvalue", "padj")
    rownames(res) <- rownames(tab)
    return(res)
}

