library(debrowser)
library(DESeq2)
library(testthat)

load(system.file("extdata", "demo", "demodata.Rda",
    package = "debrowser"))
columns <- c("exper_rep1", "exper_rep2", "exper_rep3",
    "control_rep1", "control_rep2", "control_rep3")
conds <- factor( c("Control", "Control", "Control",
    "Treat", "Treat", "Treat") )
data <- data.frame(demodata[, columns])

test_that("Able to run DESeq2", {
    deseqrun <- runDESeq(data, columns, conds)
    expect_true(exists("deseqrun"))
    expect_equal(deseqrun[[2]][[1]], 0.1641255385)
})

test_that("Linked brush initialization", {
    expect_silent( lb_scat <- linked_brush_scatter())
    expect_true(exists("lb_scat"))

    expect_silent( lb_volc <- linked_brush_volcano())
    expect_true(exists("lb_volc"))

    expect_silent( lb_ma <- linked_brush_ma() )
    expect_true(exists("lb_ma"))
})

##################################################
deseqrun <- runDESeq(data, columns, conds)
lb_scat <- linked_brush_scatter()
lb_volc <- linked_brush_volcano()
lb_ma <- linked_brush_ma()

de_res <- data.frame(deseqrun)
norm_data <- getNormalizedMatrix(data[, columns])
rdata <- cbind(rownames(de_res), norm_data[rownames(de_res), columns],
                log10(rowMeans(norm_data[rownames(de_res),
                paste(c("exper_rep1", "exper_rep2", "exper_rep3"))])
                + 0.1), log10( rowMeans( norm_data[ rownames( de_res ),
                paste(c("control_rep1", "control_rep2", "control_rep3"))])
                + 0.1), de_res[rownames(de_res),
                c("padj", "log2FoldChange")], 2 ^ de_res[rownames(de_res),
                "log2FoldChange"], -1 *
                log10(de_res[rownames(de_res), "padj"]))
colnames(rdata) <- c("ID", columns, "Cond1", "Cond2", "padj",
                "log2FoldChange", "foldChange", "log10padj")
rdata <- as.data.frame(rdata)
rdata$padj[is.na(rdata$padj)] <- 1

padj_cutoff <- 0.6
foldChange_cutoff <- 6

rdata$Legend <- character(nrow(rdata))
rdata$Legend[rdata$log2FoldChange > log2(foldChange_cutoff) &
        rdata$padj < padj_cutoff] <- "Up"
rdata$Legend[rdata$log2FoldChange < log2(1 / foldChange_cutoff) &
        rdata$padj < padj_cutoff] <- "Down"
rdata$Legend[abs(rdata$log2FoldChange) <= 
        log2(foldChange_cutoff)] <- "NS"
rdata$Legend[is.null(rdata$log10padj)] <- "NA"

dat <- rdata
dat$M <- rdata$Cond1 - rdata$Cond2
dat$A <- (rdata$Cond1 + rdata$Cond2) / 2
##################################################

test_that("plots produce no errors", {
    expect_silent( all2all(data) )

    heatmap <- runHeatmap(mtcars)
    expect_false( is.null(heatmap) )
    expect_silent( MAP <- MAPlot(dat, lb_ma) )
    expect_false( is.null(MAP) )

    expect_silent( test_scat <- mainScatter(rdata, lb_scat) )
    expect_false(is.null(test_scat))
    expect_silent( test_scat_zoom <- scatterZoom(rdata) )
    expect_false(is.null(test_scat_zoom))

    expect_silent( test_volc <- volcanoPlot(rdata, lb_volc) )
    expect_false(is.null(test_volc))
    expect_silent( test_volc_zoom <- volcanoZoom(rdata) )
    expect_false(is.null(test_volc_zoom))

    expect_silent( test_ma <- MAPlot(dat, lb_ma) )
    expect_false(is.null(test_ma))
    expect_silent( test_ma_zoom <- MAZoom(dat) )
    expect_false(is.null(test_ma_zoom))
})

test_that("plots produce no errors", {
    goInput <- NULL
    goInput$gofunc <- "groupGO"
    goInput$goplot <- "enrichGO"
    goInput$goextplot <- "Summary"
    goInput$gopvalue <- 0.01
    goInput$ontology <- "CC"
    dataset <- rdata[, columns]
    genelist <- getGeneList(rownames(dataset))
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$ontology <- "MF"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$ontology <- "BP"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$goplot <- "enrichKEGG"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$goextplot <- "Dotplot"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$goplot <- "disease"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$goextplot <- "Summary"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$goplot <- "compare"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$ontology <- "MF"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
    goInput$ontology <- "CC"
    gotest <- getGOPlots(dataset[, columns], goInput, genelist)
    expect_false(is.null(gotest))
})
