#' GOTerm analysis functions
#'
#' @note \code{GOTerm}
#'
#' @export
#' @import org.Hs.eg.db
#' @import annotate
#' @import AnnotationDbi
#' @importFrom DOSE enrichDO
#'
#' @note \code{getGeneList}
#' symobol to ENTREZ ID conversion
#' @param genes, gene list
#' @return ENTREZ ID list
#'
#' @examples
#'     x <- getGeneList(c('OCLN', 'ABCC2'))
#'

getGeneList <- function(genes) {
    ll <- org.Hs.egSYMBOL2EG
    # Get the entrez gene identifiers that are mapped to a gene symbol
    mapped_genes <- mappedkeys(ll)
    upg <- unique(mapped_genes[toupper(mapped_genes) %in% toupper(genes)])
    # Convert to a list
    genelist <- rapply(cbind(as.list(ll[upg])), c)
    return(genelist)
}


#' getEnrichGO
#'
#' @note \code{getEnrichGO}
#' @param genelist, gene list
#' @param pvalueCutoff, p value cutoff
#' @param org, the organism used 'org.Hs.eg.db'
#' @param ont, the ontology used
#' @return Enriched GO
#' @examples
#'     genelist<-getGeneList(c('OCLN', 'ABCC2'))
#'     x <- getEnrichGO(genelist, 0.01, NULL, "CC")
#'
#' @export
#'

getEnrichGO <- function(genelist, pvalueCutoff = 0.01,
    org = "org.Hs.eg.db", ont="CC") {
    if (is.null(org)) return(NULL)
    res <- c()
    res$enrich_p <- enrichGO(gene = genelist, OrgDb = org,
            ont = ont, pvalueCutoff = pvalueCutoff,
            readable = TRUE)

    res$p <- barplot(res$enrich_p, title = paste("Enrich GO", ont),
            font.size = 12)
    return(res)
}

#' getEnrichKEGG
#'
#' @note \code{getEnrichKEGG}
#' @param genelist, gene list
#' @param pvalueCutoff, the p value cutoff
#' @return Enriched KEGG
#' @examples
#'     genelist<-getGeneList(c('OCLN', 'ABCC2'))
#'     x <- getEnrichKEGG(genelist,NULL)
#'
#' @export
#'

getEnrichKEGG <- function(genelist, pvalueCutoff = 0.01) {
    if (is.null(pvalueCutoff)) return(NULL)
    res <- c()
    res$enrich_p <- enrichKEGG(gene = genelist, pvalueCutoff = pvalueCutoff,
        use_internal_data = TRUE)
    res$p <- barplot(res$enrich_p, title = paste("KEGG Enrichment: p-value=",
        pvalueCutoff))
    return(res)
}

#' clusterData
#'
#' @note \code{clusterData}
#' @param dat, the data to cluster
#' @return clustered data
#' @examples
#'     mycluster <- clusterData(mtcars)
#'
#' @export
#'
clusterData <- function(dat) {
    norm_data <- getNormalizedMatrix(dat)
    mydata <- na.omit(norm_data)  # listwise deletion of missing
    mydata <- scale(mydata)  # standardize variables

    wss <- (nrow(mydata) - 1) * sum(apply(mydata, 2, var))
    for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers = i)$withinss)
    plot(1:15, wss, type = "b",
        xlab = "Number of Clusters",
        ylab = "Within groups sum of squares")
    k <- 0
    for (i in 1:14) {
        if ( ( wss[i] / wss[i + 1] ) > 1.2 ) {
            k <- k + 1
        }
    }
    # K-Means Cluster Analysis
    fit <- kmeans(mydata, k)  # 5 cluster solution
    # get cluster means
    aggregate(mydata, by = list(fit$cluster), FUN = mean)
    # append cluster assignment
    mydata_cluster <- data.frame(mydata, fit$cluster)

    return(mydata_cluster)
}


#' compareClust
#'
#' @note \code{compareClust}
#' @param dat, data to compare clusters
#' @param ont, the ontology to use
#' @param org, the organism used
#' @param fun, fun
#' @param title, title of the comparison
#' @return compared cluster
#' @examples
#'     x <- compareClust()
#'
#' @export
#'

compareClust <- function(dat = NULL, ont = "CC", org = "org.Hs.eg.db",
    fun = "enrichGO", title=NULL) {
    if (is.null(dat)) return(NULL)
    if (is.null(title))
        title = "Ontology Distribution Comparison"
    genecluster <- list()
    k <- max(dat$fit.cluster)
    for (i in 1:k) {
        clgenes <- rownames(dat[dat$fit.cluster == i, ])
        genelist <- getGeneList(clgenes)
        genecl <- list()
        genecl <- push(genecl, genelist)
        genecluster[c(paste("X", i, sep = ""))] <- genecl
    }

    p <- tryCatch({
        title <- paste(fun, title)
        xx <- c()
        if (fun == "enrichKEGG" || fun == "enrichPathway")
            xx <- compareCluster(genecluster, fun = fun,
                                OrgDb = org)
        else if (fun == "enrichDO")
            xx <- compareCluster(genecluster, fun = fun)
        else {
            xx <- compareCluster(genecluster, fun = fun,
                                OrgDb = org, ont = ont)
            title <- paste(ont, title)
        }
            p <- plot(xx, title = title)
            p
    })
    p
}

#' getEnrichDO
#'
#' @note \code{getEnrichDO}
#' @param genelist, gene list
#' @param pvalueCutoff, the p value cutoff
#' @return enriched DO
#' @examples
#'     genelist<-getGeneList(c('OCLN', 'ABCC2'))
#'     x <- getEnrichDO(genelist, NULL)
#'
#' @export
#'
getEnrichDO <- function(genelist, pvalueCutoff = 0.01) {
    if (is.null(pvalueCutoff)) return(NULL)
    res <- c()
    res$enrich_p <- enrichDO(gene = genelist, ont = "DO",
        pvalueCutoff = pvalueCutoff, readable = TRUE)

    res$p <- barplot(res$enrich_p, title = "Enrich DO", font.size = 12)
    res
}
