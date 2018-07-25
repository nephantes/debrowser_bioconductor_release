#' getIntroText
#' Intro text
#'
#' @return the JS for tab updates
#'
#' @examples
#'     x<- getIntroText()
#'
#' @export
getIntroText<-function(){

list(
h2("Quick Start Guide"),
p("Differential expression (DE) analysis has become an increasingly popular tool
in determining and viewing up and/or down experssed genes between two sets of
samples."),
p("The goal of DEBrowser is to provide an easy way to perform and visualize DE analysis."),
#####################################################
##Data input section
h3("1. Data input"),
p("The input data for DEBrowser is 2 files in '.txt', '.csv' or '.tsv' format, 
  they are named as 'Count Data File' and 'Metadata File'."),
h4("1.1 Count Data File"),            
p("This input file should contain summarized count results of all samples in the experiment, 
  an example of the expected input data format is presented as below:"),
tableOutput("countFile"),
p("Where columns are samples, rows are the mapped genomic features 
  (e.g. genes, isoforms, miRNAs, ATAC or Chip regions etc.)."),
p("If you do not have a dataset to upload, you can use the built in demo data file by 
  clicking on the ‘Load Demo’ buttons from two different publications. To view the entire demo data file, you can 
  download the ", a("demo set 1 (Vernia et. al).", href="https://bioinfo.umassmed.edu/pub/debrowser/simple_demo.tsv"),
  "For another example, try our ", a("full dataset for demo set 1 (Vernia et. al)", href="https://bioinfo.umassmed.edu/pub/debrowser/advanced_demo.tsv")),
h4("1.2 Metadata File"),
p("In addition to the count data file; you may also upload metadata file to correct for 
batch effects or any other normalizing conditions you might want to address that might be
within your results. To handle for these conditions, simply create a metadata file by 
using the example table at below "),
tableOutput("metaFile"),
p("or download sample metadata file used for:",
a("metadata file for set 1 (Vernia et. al).", href="https://bioinfo.umassmed.edu/pub/debrowser/simple_demo_meta.txt")),
p("To be able to upload the data please press",  
actionButton("UploadBut", label = "Upload", styleclass = "primary"), "button in the data upload page."),
p("After sucessfull upload, you should see the summary of your data in the 'Upload Summary' section. 
  To move the filtering section please click",  
actionButton("FilterBut", label = "Filter", styleclass = "primary"), "button in the upload page."),
p("If you are ready to use DEBrowser, please click 'Upload' menu on the left to start using DEBrowser"))
}

#' getDataAssesmentText
#' DataAssesment text
#'
#' @return help text for data assesment 
#'
#' @examples
#'     x<- getDataAssesmentText()
#'
#' @export
getDataAssesmentText<-function(){
list(
h3("2. Data Assesment"),
h4("2.1 Low Count Filtering"),    
p("In this section, you can simultaneously visualise the changes of your dataset 
    while filtering out the low count genes. Choose your filtration criteria from 
    Filtering Methods box which is located just center of the screen. 
    Three methods are available to be used:"),
p(strong("Max:"), "Filters out genes where maximum count for each gene across all samples are less 
   than defined threshold."),
p(strong("Mean:"), "Filters out genes where mean count for each gene are less than defined threshold."),
p(strong("CPM:"), "First, counts per million (CPM) is calculated as the raw counts divided by the 
    library sizes and multiplied by one million. Then it filters out genes where at least 
    defined number of samples is less than defined CPM threshold."),
withMathJax(),
p("The expression cutoff value is determined according to the library size 
    and normalization factors with formula $$\\text{CPM} = \\frac{\\text{raw counts}}{\\text{library size} * \\text{normalization factors} * 10^{-6}}$$ 
    For example, if the cutoff CPM value is 10, 
    the library size and normalization factors are estimated approximately equal to \\(\\ 3 \\text{ x} 10 ^ 6\\) and 1 for at least 4 samples, 
    then 10 CPM expression cutoff corresponds to about 30 read counts. 
    Therefore, in this example features in more than 4 samples have less than 
    30 read counts (10 CPM) is going to be low expression features
    and will be removed for batch effect correction and DE analysis."),
p("To be able to filter out the low expression counts please press",  
    actionButton("FilterBut", label = "Filter", styleclass = "primary"), "button in data filtering page."),
h4("2.2 Quality control(QC)"),
p("After filtering low count features, you may continue your analysis with Batch Effect 
    Detection & Correction or directly jump to differential expression analysis or view 
    quality control (QC) information of your dataset."),
p("If specified metadata file containing your treatment and batch fields, by clicking ", 
    actionButton("BatchBut", label = "Batch effect correction", styleclass = "primary"), 
    "button, you have the option to conduct PCA, interquartile range (IQR) and density 
    plots to asses if the data requires batch effect correction or not."),
p("If user wants to skip batch effect assesment and correction step, they can either click",
   actionButton("goDEFromFilterBut", "Go to DE Analysis", styleclass = "primary"),  
   " button to perform DE Analysis or ",
   actionButton("goQCplotsFromFilterBut", "Go to QC plots", styleclass = "primary"), 
  " button for QC plots to draw PCA, all2all scatter, heatmaps, IQR and density plots.")
)
}

#' getDataPreparationText
#' DataPreparation text
#'
#' @return help text for data preparation 
#'
#' @examples
#'     x<- getDataPreparationText()
#'
#' @export
getDataPreparationText<-function(){
list(
    h3("3. Data Preparation"),
    p("With metadata file containing your batch correction fields 
    then you have the option to conduct ",strong("batch effect correction"), " prior to 
    your analysis. By adjusting parameters of Options box, you can investigate 
    your character of your dataset. These parameters of the options box are 
    explained as following:"),
    p(strong("Normalization Method:"), "DEBrowser allows performing normalization 
    prior the batch effect correction. You may choose your normalization method 
    (among MRE, TMM, RLE, upperquartile), or if you don’t want to normalize your 
    data you can select none for this item."),
    p(strong("Correction Method:"), "DEBrowser uses ComBat (part of the SVA 
    bioconductor package) or Harman to adjust for possible batch effect or conditional 
    biases. For more information, you can visit following links for 
    documentation: ComBat, Harman"),
    p(strong("Treatment:"), "Please select the column that is specified in 
    metadata file for comparision, such as cancer vs control. It is named 
    condition for our sample metadata."),
    p(strong("Batch:"), "Please select the column name in metadata file 
    which differentiate the batches. For example in our metadata, it is called batch.
    Upon clicking submit button, comparison tables and plots will be created on the right 
    part of the screen as shown below."),
    p("You can investigate the changes on the data by comparing following features:"),
    p("1. Read counts for each sample.",br(),
    "2. PCA, IQR and Density plot of the dataset.", br(),
    "3. Gene/region vs samples data"),
    p("After batch effect correction, user can click ",
    actionButton("goDEFromFilterBut", "Go to DE Analysis", styleclass = "primary"),  
    " button to perform DE Analysis or ",
    actionButton("goQCplotsFromFilterBut", "Go to QC plots", styleclass = "primary"), 
    " button for QC plots to draw PCA, all2all scatter, heatmaps, IQR and density plots.")
)
}
#' getDEAnalysisText
#' DEAnalysis text
#'
#' @return help text for DE Analysis 
#'
#' @examples
#'     x<- getDEAnalysisText()
#'
#' @export
getDEAnalysisText<-function(){
list(
h3("4. DE analysis"),
p("The goal of differential gene expression analysis is to find genes
or transcripts whose difference in expression, when accounting for the
variance within condition, is higher than expected by chance."),
p(a("DESeq2", href="https://bioconductor.org/packages/release/bioc/html/DESeq2.html"),
"is an R package available via Bioconductor and is designed to normalize count
data from high-throughput sequencing assays such as RNA-Seq and test for
differential expression (Love et al. 2014).  With multiple parameters such as
padjust values, log fold changes, plot styles, and so on, altering plots
created with your DE data can be a hassle as well as time consuming. The
Differential Expression Browser uses DESeq2 (Love et al., 2014)",
a("EdgeR",href="https://bioconductor.org/packages/release/bioc/html/edgeR.html"),
"(Robinson et al., 2010), and ",
a("Limma", href="https://bioconductor.org/packages/release/bioc/html/limma.html"),
"(Ritchie et al., 2015) coupled with shiny (Chang, W. et al., 2016)
to produce real-time changes within your
plot queries and allows for interactive browsing of your DE results.
In addition to DE analysis, DEBrowser also offers a variety of other plots
and analysis tools to help visualize your data even further."),
p("If you are ready to discover and visualize your data, please click ",
  actionButton("mainPlotsBut", "Go to Main Plots", styleclass = "primary"), 
  "button in DE Results section."),
h4("4.1 Used parameters for DESeq2"),
p(strong("fitType:")),
p("Either 'parametric', 'local', or 'mean' for the type of fitting of 
dispersions to the mean intensity. See estimateDispersions for description."),
p(strong("betaPrior:")),
p("Whether or not to put a zero-mean normal prior on the non-intercept 
coefficients See nbinomWaldTest for description of the calculation of 
the beta prior. By default, the beta prior is used only for the Wald test, 
but can also be specified for the likelihood ratio test."),
p(strong("testType:")),
p("Either 'Wald' or 'LRT', which will then use either Wald significance tests 
(defined by nbinomWaldTest), or the likelihood ratio test on the difference in 
deviance between a full and reduced model formula (defined by nbinomLRT)"),
h4("4.2 Used parameters for EdgeR"),
p(strong("Normalization:")),
p("Calculate normalization factors to scale the raw library sizes. Values 
can be 'TMM','RLE','upperquartile','none'."),
p(strong("Dispersion:")),
p("Either a numeric vector of dispersions or a character string indicating 
that dispersions should be taken from the data object."),
p(strong("testType:")),
p("ExactTest or glmLRT. ",strong("exactTest:")," Computes p-values for differential 
abundance for each gene between two samples, conditioning on the total 
count for each gene. The counts in each group are assumed to follow a 
binomial distribution. ",strong("glmLRT:")," Fits a negative binomial generalized 
log-linear model to the read counts for each gene and conducts 
genewise statistical tests."),
h4("4.3 Used parameters for Limma"),
p(strong("Normalization:")),
p("Calculate normalization factors to scale the raw library sizes. 
Values can be 'TMM','RLE','upperquartile','none'."),
p(strong("Fit Type:")),
p("fitting method; “ls” for least squares or “robust” for robust regression"),
p(strong("Norm. Bet. Arrays:")),
p("Normalization Between Arrays; Normalizes expression intensities so that the 
intensities or log-ratios have similar distributions across a set of arrays.")
)
}
#' getQAText
#' Some questions and answers
#'
#' @return help text for QA 
#'
#' @examples
#'     x<- getQAText()
#'
#' @export
getQAText<-function(){
    list(
        h3("5. Questions and Answers"),
        h4("5.1 Why un-normalized counts?"),
        p("DESeq2 requires count data as input obtained from 
          RNA-Seq or another high-thorughput sequencing experiment 
          in the form of matrix values. Here we convert un-integer 
          values to integer to be able to run DESeq2. The matrix values 
          should be un-normalized, since DESeq2 model internally corrects for 
          library size. So, transformed or normalized values such as counts 
          scaled by library size should not be used as input. Please use edgeR 
          or limma for normalized counts.")
    )
}