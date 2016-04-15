#' Run DESeq2 algorithm.
#'
#' @param data, A matrix that includes all the expression raw counts,
#'     rownames has to be the gene, isoform or region names/IDs
#' @param columns, is a vector that includes the columns that are going
#'     to be analyzed. These columns has to match with the given data.
#' @param conds, experimental conditions. The order has to match
#'     with the column order
#' @param fitType, DESeq2 fitType, it can be 'parametric', 
#'     'local', 'mean'.
#' @param non_expressed_cutoff, to remove unexpressed 
#'     regions/genes/isoforms this cutoff is used
#' @return deseq2 results
#'
#' @export
#'
#' @examples
#'     x <- runDESeq(data<-NULL, columns<-c())
#'

runDESeq <- function(data, columns, conds,
    fitType = c("parametric", "local", "mean"),
    non_expressed_cutoff = 10) {
        data <- data[, columns]
        if ( !is.null(data) ){
            data[, columns] <- apply(data[, columns], 2,
                function(x) as.integer(x))

            conds <- factor(conds)

            coldata <- data.frame(colnames(data))
            coldata <- cbind(coldata, conds)
            colnames(coldata) <- c("libname", "group")
            # Filtering non expressed genes
            filtd <- subset(data, rowSums(data) > non_expressed_cutoff)

            # DESeq data structure is going to be prepared
            dds <- DESeqDataSetFromMatrix(countData = as.matrix(filtd),
                colData = coldata, design = ~group)

            # Running DESeq
            dds <- DESeq(dds, fitType = fitType)
            res <- results(dds)

            return(res)
        }
}
