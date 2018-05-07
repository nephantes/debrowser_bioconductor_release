#' debrowsercondselect
#'
#' Condition selection
#' This is not a module. Module construction didn't used here, just use it 
#' as functions not in a module.
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, count data
#' @param metadata, metadata
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowsercondselect()
#'
debrowsercondselect <- function(input, output, session, data, metadata=NULL) {

    choicecounter <- reactiveValues(nc = 0)
    
    output$conditionSelector <- renderUI({
        selectConditions(Dataset = data,
            metadata = metadata,
            choicecounter = choicecounter,
            input = input,
            session = session)
    })
    observeEvent(input$add_btn, {
        choicecounter$nc <- choicecounter$nc + 1
    })
    observeEvent(input$rm_btn, {
        if (choicecounter$nc > 0) 
            choicecounter$nc <- choicecounter$nc - 1
    })
    cc <- reactive({
        choicecounter$nc
    })
    list(cc = cc)
}

#' condSelectUI
#' Creates a panel to select samples for each condition
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- condSelectUI()
#'
#' @export
#'
condSelectUI<- function () {
list(
    shinydashboard::box(title = "Comparison Selection",
        solidHeader = T, status = "info",  width = NULL, height = NULL, collapsible = TRUE,
    fluidRow(
        uiOutput("conditionSelector"),
        column(12,actionButton("add_btn", "Add New Comparison",styleclass = "primary"),
               actionButton("rm_btn", "Remove", styleclass = "primary"),
               getHelpButton("method", "http://debrowser.readthedocs.io/en/develop/deseq/deseq.html"),
               actionButton("startDE", "Start DE!", styleclass = "primary"))
    ))
)
}
#getMethodDetails
#'
#' get the detail boxes after DE method selected 
#'
#' @param num, panel that is going to be shown
#' @param input, user input
#' @examples
#'     x <- getMethodDetails()
#'
#' @export
#'
#'
getMethodDetails <- function(num = 0, input = NULL) {
    if (num > 0)
        a <- list(
            conditionalPanel(
                (condition <- paste0("input.demethod",num," == 'DESeq2'")),
                getSelectInputBox("fitType", "Fit Type", num, 
                                c("parametric", "local", "mean"), 
                                selectedInput("testType", num, "parametric",
                                                input), 3),
                getSelectInputBox("betaPrior", "betaPrior", num, 
                                            c(F, T), 
                                            selectedInput("betaPrior", num,
                                                          F, input),2),
                getSelectInputBox("testType", "Test Type", num, 
                            c("Wald", "LRT"),  
                            selectedInput("testType", num, "Wald", input))),
            conditionalPanel(
                (condition <- paste0("input.demethod",num," == 'EdgeR'")),
                getSelectInputBox("edgeR_normfact", "Normalization", num, 
                        c("TMM","RLE","upperquartile","none"), 
                        selectedInput("edgeR_normfact", num, "TMM", input), 3),
                column(2,textInput(paste0("dispersion", num), "Dispersion", 
                                value = isolate(selectedInput("dispersion", 
                                num, "0", input) ))),
                getSelectInputBox("edgeR_testType", "Test Type", num, 
                                  c("exactTest", "glmLRT"), 
                                  selectedInput("edgeR_testType", num,
                                                "exactTest", input))),
            conditionalPanel(
                (condition <- paste0("input.demethod",num," ==  'Limma'")),
                getSelectInputBox("limma_normfact", "Normalization", num, 
                        c("TMM","RLE","upperquartile","none"), 
                        selectedInput("limma_normfact", num, "TMM", input), 3),
                getSelectInputBox("limma_fitType", "Fit Type", num,
                        c("ls", "robust"), 
                        selectedInput("limma_fitType", num, "ls", input)),
                getSelectInputBox("normBetween", "Norm. Bet. Arrays", num,
                        c("none", "scale", "quantile", "cyclicloess",
                        "Aquantile", "Gquantile", "Rquantile","Tquantile"),
                        selectedInput("normBetween", num, "none", input))),
            br())
}

#' getConditionSelector
#'
#' Selects user input conditions to run in DESeq.
#'
#' @param num, panel that is going to be shown
#' @param choices, sample list
#' @param selected, selected smaple list
#' @examples
#'     x <- getConditionSelector()
#'
#' @export
#'
getConditionSelector<- function(num=0, choices = NULL, selected = NULL) {
    if (!is.null(choices))
        a <- list(column(6, selectInput(paste0("condition", num),
            label = paste0("Condition ", num),
            choices = choices, multiple = TRUE,
            selected = selected)))
}

#' getConditionSelectorFromMeta
#'
#' Selects user input conditions to run in DESeq from metadata
#'
#' @param metadata, meta data table
#' @param input, input
#' @param index, index
#' @param num, num
#' @param choices, choices
#' @param selected, selected
#' 
#' @examples
#'     x <- getConditionSelectorFromMeta()
#'
#' @export
#'
getConditionSelectorFromMeta <- function(metadata = NULL, input = NULL, index = 1, num=0, 
    choices = NULL, selected = NULL) {
     a <- list(column(6, selectInput(paste0("condition", num),
            label = paste0("Condition ", num),
            choices = choices, multiple = TRUE,
            selected = selected)))

     if (!is.null(metadata)){
        selected_meta <- selectedInput("conditions_from_meta", 
            index, NULL, input)
        
        if (is.null(selected_meta)) selected_meta <- "No Selection"
    
        if (selected_meta != "No Selection"){
            old_selection <- ""
            
        if(!is.null(input[[paste0("condition", num)]])){
            selected <- input[[paste0("condition", num)]]
        } 
        meta_choices_all <- NULL
        if (!is.null(selected_meta))
            meta_choices_all <- get_conditions_given_selection(metadata,
                                        selected_meta)
        if(old_selection != selected_meta){
            if(typeof(meta_choices_all) == "character"){
                meta_choices <- list("There must be exactly 2 groups.")
            } else{
                meta1 <- meta_choices_all[[2 - (num %% 2)]]
                meta_choices <- unlist(meta1, recursive=FALSE)
            }
            selected <- meta_choices
        }
    
        a <- list(column(6, selectInput(paste0("condition", num),
            label = paste0("Condition ", num),
            choices = choices, multiple = TRUE,
            selected = selected)))
        }
    }
    return(a)
}

#' selectedInput
#'
#' Selects user input conditions to run in DESeq.
#'
#' @param id, input id
#' @param num, panel that is going to be shown
#' @param default, default text
#' @param input, input params
#' @examples
#'     x <- selectedInput()
#'
#' @export
#'
selectedInput <- function(id = NULL, num = 0, default = NULL, 
                          input = NULL) {
    if (is.null(id)) return(NULL)
    m <- NULL
    if (is.null(input[[paste0(id, num)]]))
        m <- default
    else
        m <- input[[paste0(id, num)]]
    m
}

#' getSelectInputBox
#'
#' Selects user input conditions to run in DESeq.
#'
#' @param id, input id
#' @param name, label of the box
#' @param num, panel that is going to be shown
#' @param choices, sample list
#' @param selected, selected smaple list
#' @param cw, column width
#' @examples
#'     x <- getSelectInputBox()
#'
#' @export
#'
getSelectInputBox <- function(id = NULL, name = NULL, 
                              num = 0, choices = NULL, selected = NULL,
                              cw = 2) {
    if (is.null(id)) return(NULL)
    if (!is.null(choices))
        a <- list(column(cw, selectInput(paste0(id, num),
            label = name,
            choices = choices, multiple = FALSE,
            selected = selected)))
}


#' selectConditions
#'
#' Selects user input conditions, multiple if present, to be
#' used in DESeq.
#'
#' @param Dataset, used dataset 
#' @param metadata, metadatatable to select from metadata
#' @param choicecounter, choicecounter to add multiple comparisons
#' @param input, input params
#' @note \code{selectConditions}
#' @return the panel for go plots;
#'
#' @examples
#'     x<- selectConditions()
#'
#' @export
#'
selectConditions<-function(Dataset = NULL,
                           metadata = NULL,
                           choicecounter = NULL,
                           input = NULL,
                           session = NULL) {
    if (is.null(Dataset)) return(NULL)
    
    selectedSamples <- function(num){
        if (is.null(input[[paste0("condition", num)]]))
            getSampleNames(colnames(Dataset), num %% 2 )
        else
            input[[paste0("condition", num)]]
    }
    nc <- choicecounter$nc
    
    if (nc >= 0) {
        if(!exists("all_selections")){
            all_selections <- ""
        }
        allsamples <- getSampleNames( colnames(Dataset), "all" )
        
        lapply(seq_len(nc), function(i) {
            selected1 <- selectedSamples(2 * i - 1)
            selected2 <- selectedSamples( 2 * i )
           
            to_return <- list(column(12, getMetaSelector(metadata = metadata, input=input, n = i),
            
                    getConditionSelectorFromMeta(metadata, input, i,
                        (2 * i - 1), allsamples, selected1),
                    getConditionSelectorFromMeta(metadata, input, i,
                        (2 * i), allsamples, selected2)
    
            ),
            
            column(12, 
                   column(1, helpText(" ")),
                   getSelectInputBox("demethod", "DE Method", i, 
                        c("DESeq2", "EdgeR", "Limma"),
                        selectedInput("demethod", i, "DESeq2", input)),
                   getMethodDetails(i, input)))
           
            return(to_return)
        })
    }
}

#' getMetaSelector
#'
#' Return the sample selection box using meta data table
#'
#' @param metadata, meta data table
#' @param input, input params
#' @param n, the box number
#' @return meta select box
#'
#' @examples
#'     x<-getMetaSelector()
#' @export
#'
getMetaSelector <- function(metadata = NULL, input = NULL, n = 0){		
    if(!is.null(metadata)){		
        df <- metadata
        col_count <- length(colnames(df))		

        list(HTML('<hr style="color: white; border:solid 1px white;">'),
             br(), column(10, selectInput(paste0("conditions_from_meta", 
                 n), label = "Select Meta",
                 choices = as.list(c("No Selection", 
                 colnames(df)[2:col_count])),
                 multiple = FALSE,
                 selected =  selectedInput("conditions_from_meta",
                 n, "Selection 2", input))))
    }
}

#' get_conditions_given_selection
#'
#' Return the two set of conditions given the selection of meta select box
#'
#' @param metadata, meta data table
#' @param selection, selection
#' @return meta select box
#'
#' @examples
#'     x<-get_conditions_given_selection()
#' @export
#'
get_conditions_given_selection <- function(metadata = NULL, selection){		
	
    if(!is.null(metadata)){		
        df <- metadata	
        if(selection == "No Selection"){		
            return(NULL)		
        }		
        if(length(levels(factor(df[,selection]))) != 2){		
            return("There must be exactly 2 groups.")		
        } else {		
            # Assuming the first column has samples		
            sample_col_name <- colnames(df)[1]		
            
            condition1 <- levels(df[,selection])[1]		
            condition2 <- levels(df[,selection])[2]		
            
            # In case the conditions are integers		
            if(is.null(condition2)){		
                condition1 <- levels(factor(df[,selection]))[1]		
                condition2 <- levels(factor(df[,selection]))[2]		
            }		
            
            condition1_filtered <- df[df[,selection] == condition1, ]		
            a <- condition1_filtered[,sample_col_name]		
            
            condition2_filtered <- df[df[,selection] == condition2, ]		
            b <- condition2_filtered[,sample_col_name]		
            
            both_groups <- list(a, b)
            return(both_groups)		
        }		
    }		
}

#' getSampleNames
#'
#' Prepares initial samples to fill condition boxes.  
#' it reads the sample names from the data and splits into two. 
#'
#' @param cnames, sample names in the header of a dataset
#' @param part, c(1,2). 1=first half and 2= second half
#' @return sample names.
#'
#' @examples
#'     x<-getSampleNames()
#' @export
#'
getSampleNames <- function(cnames = NULL, part = 1) {
    if (is.null(cnames)) return(NULL)
    
    startpos <- 1
    endpos <- length(cnames)
    if (part == 1)
        endpos <- floor(endpos / 2) 
    else if (part == 0)
        startpos <- floor(endpos / 2) + 1
    
    cn <- cnames[startpos:endpos]
    m <- as.list(NULL)
    for (i in seq(cn)) {
        m[[i]] <- cn[i]
    }
    m
}

#' prepDataContainer
#'
#' Prepares the data container that stores values used within DESeq.
#'
#' @param data, loaded dataset
#' @param counter, the number of comparisons
#' @param input, input parameters
#' @return data
#' @export
#'
#' @examples
#'     x <- prepDataContainer()
#'
prepDataContainer <- function(data = NULL, counter=NULL, 
                              input = NULL) {
    if (is.null(data)) return(NULL)
    
    inputconds <- reactiveValues(demethod_params = list(), conds = list(), dclist = list())
    observeEvent(input$startDE, {
        inputconds$conds <- list()
        for (cnt in seq(1:(2*counter))){
            inputconds$conds[cnt] <- list(isolate(input[[paste0("condition",cnt)]]))
        }
        #Get parameters for each method
        inputconds$demethod_params <- NULL
        for (cnt in seq(1:counter)){
            if (isolate(input[[paste0("demethod",cnt)]]) == "DESeq2"){
                inputconds$demethod_params[cnt] <- paste(
                    isolate(input[[paste0("demethod",cnt)]]),
                    isolate(input[[paste0("fitType",cnt)]]),
                    isolate(input[[paste0("betaPrior",cnt)]]),
                    isolate(input[[paste0("testType",cnt)]]), sep=",")
            }
            else if (isolate(input[[paste0("demethod",cnt)]]) == "EdgeR"){
                inputconds$demethod_params[cnt]<- paste(
                    isolate(input[[paste0("demethod",cnt)]]),
                    isolate(input[[paste0("edgeR_normfact",cnt)]]),
                    isolate(input[[paste0("dispersion",cnt)]]),
                    isolate(input[[paste0("edgeR_testType",cnt)]]), sep=",")
            }
            else if (isolate(input[[paste0("demethod",cnt)]]) == "Limma"){
                inputconds$demethod_params[cnt] <- paste(
                    isolate(input[[paste0("demethod",cnt)]]),
                    isolate(input[[paste0("limma_normfact",cnt)]]),
                    isolate(input[[paste0("limma_fitType",cnt)]]),
                    isolate(input[[paste0("normBetween",cnt)]]), sep=",")
            }
        }
        
        for (i in seq(1:counter))
        {
            conds <- c(rep(paste0("Cond", 2*i-1), 
                           length(inputconds$conds[[2*i-1]])), 
                       rep(paste0("Cond", 2*i), length(inputconds$conds[[2*i]])))
            cols <- c(paste(inputconds$conds[[2*i-1]]), 
                      paste(inputconds$conds[[2*i]]))
            params <- unlist(strsplit(inputconds$demethod_params[i], ","))
            withProgress(message = 'Running DE Algorithms', detail = inputconds$demethod_params[i], value = 0, {
                initd <- callModule(debrowserdeanalysis, paste0("DEResults",i), data = data, 
                      columns = cols, conds = conds, params = params)
                if (nrow(initd$dat()) > 1){
                    inputconds$dclist[[i]] <- list(conds = conds, cols = cols, init_data=initd$dat(), 
                        demethod_params = inputconds$demethod_params[i])
                }
                incProgress(1/counter)
            })
        }
    })

    comparison <- reactive({
        compselect <- 1
        dat <- NULL
        if(length(inputconds$dclist) <1) return(NULL)
        if (!is.null(input$compselect))
            compselect <- as.integer(input$compselect)
        if (length(inputconds$dclist) > 1 && !is.null(inputconds$dclist[[compselect]])){
            dat <- inputconds$dclist[[compselect]]
        }
        dat
    })
    
    list(comp = comparison)
}