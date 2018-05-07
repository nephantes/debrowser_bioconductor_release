#' getSampleDetails
#' 
#' get sample details
#'
#' @param output, output
#' @param summary, summary output name
#' @param details, details ouput name
#' @param data, data 
#' @return panel
#' @examples
#'     x <- getSampleDetails(output, data)
#'
#' @export
#'
getSampleDetails<- function (output, summary, details, data) {
    if (is.null(data$count)) return(NULL)
    
    output[[summary]]<- renderTable({ 
        countdata <-  data$count
        samplenums <- length(colnames(countdata))
        rownums <- dim(countdata)[1]
        result <- rbind(samplenums, rownums)
        rownames(result) <- c("# of samples", "# of rows (genes/regions)")
        colnames(result) <- "Value"
        result
    },digits=0, rownames = TRUE, align="lc")
    
    output[[details]] <- DT::renderDataTable({ 
        dat <- colSums(data$count)
        dat <- cbind(names(dat), dat)
        dat[, c("dat")] <-  format(
            round( as.numeric( dat[,  c("dat")], digits = 2)),
            big.mark=",",scientific=FALSE)
        
        if (!is.null(data$meta)){
            met <- data$meta
            dat <- cbind(met, dat[,"dat"])
            rownames(dat) <- NULL
            colnames(dat)[ncol(dat)] <- "read counts"
        }else{
            rownames(dat) <- NULL
            colnames(dat) <- c("samples", "read counts")
        }
        dat
    })
}

#' selectGroupInfo
#'
#' Group info column selection. This can be used in batch effect
#' or coloring the groups in the plots.
#'
#' @param input, input values
#' @param selectname, name of the select box
#' @param label, label of the select box
#' @note \code{selectGroupInfo}
#' @examples
#'     x <- selectGroupInfo()
#' @export
#'
selectGroupInfo <- function(metadata = NULL, input = NULL,
                              selectname = "groupselect",
                              label = "Group info") {
    if (is.null(metadata)) return (NULL)
    lst.choices <- as.list(c("None", colnames(metadata)))
    selectInput(selectname, label = label,
                choices = lst.choices,
                selected = 1)
}


#' addID
#'
#' Adds an id to the data frame being used.
#'
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

#' getVariationData
#'
#' Adds an id to the data frame being used.
#'
#' @param inputdata, dataset 
#' @param cols, columns
#' @param conds, conditions
#' @param key, gene or region name
#' @return plotdata
#' @export
#'
#' @examples
#'     x <- getVariationData()
#'
getVariationData <- function(inputdata = NULL, 
    cols = NULL, conds = NULL, key = NULL) {
    # Pick out the gene with this ID
    vardata <- inputdata[key, ]
    bardata <- as.data.frame(cbind(key, cols,
         t(vardata[, cols]), conds) )
    colnames(bardata) <- c("genename", "libs", "count", "conds")
    bardata$count <- as.numeric(as.character(bardata$count))
    data <- rbind(bardata[bardata$conds == levels(bardata$conds)[1], ],
                  bardata[bardata$conds == levels(bardata$conds)[2], ])
    data$conds  <- factor(data$conds, levels = unique(data$conds))
    data
}

#' getBSTableModal
#' prepares a Modal to put a table
#'
#' @return the modal
#'
#' @examples
#'     x<- getBSTableModal()
#'
#' @export
getBSTableUI<-function(name,  label, trigger, size="large", modal = NULL){
    ret <- div(style = "display:block;overflow-y:auto; overflow-x:auto;",
               wellPanel( DT::dataTableOutput(name)))
    if (!is.null(modal) && modal)
        ret <- shinyBS::bsModal(name, label, trigger, size = size, ret)
    ret
}

#' getTableDetails
#' 
#' get table details
#' To be able to put a table into two lines are necessary;
#' into the server part;
#' getTableDetails(output, session, "dataname", data, modal=TRUE)
#' into the ui part;
#' uiOutput(ns("dataname"))
#'   
#' @param output, output
#' @param session, session
#' @param tablename, table name
#' @param data, matrix data
#' @param modal, if it is true, the matrix is going to be in a modal
#' @return panel
#' @examples
#'     x <- getTableDetails()
#'
#' @export
#'
getTableDetails <- function(output, session, tablename, data = NULL, modal = NULL){
    if (is.null(data)) return(NULL)
    tablenameUI <-  paste0(tablename,"Table")
    output[[paste(tablename, "Download")]] <- downloadHandler(
        filename = function() {
            paste0(tablename,".tsv")
        },
        content = function(file) {
            if(!("ID" %in% names(data)))
                data <- addID(data)
            write.table(data, file, sep = "\t", row.names = FALSE)
        }
    )
    
    output[[tablename]] <- renderUI({
        ret <- getBSTableUI( session$ns(tablenameUI), "Show Data", paste0("show",tablename), modal = modal) 
        if (!is.null(modal) && modal)
           ret <- list( downloadButton(session$ns(paste(tablename, "Download")), "Download"),
               actionButton(paste0("show",tablename), "Show Data", styleclass = "primary", icon="show"),
               ret)
        ret    
    })
    
    output[[tablenameUI]] <- DT::renderDataTable({
        if (!is.null(data)){
            DT::datatable(data, extensions = 'Buttons',
            options = list( server = TRUE,
            dom = "Blfrtip",
            buttons = 
              list("copy", list(
                  extend = "collection"
                  , buttons = c("csv", "excel", "pdf")
                  , text = "Download"
              ) ), # end of buttons customization
            
            # customize the length menu
            lengthMenu = list( c(10, 20,  50, -1) # declare values
                               , c(10, 20, 50, "All") # declare titles
            ), # end of lengthMenu customization
            pageLength = 10))
        }
    })
}

#' push
#'
#' Push an object to the list.
#'
#' @param l, that are going to push to the list
#' @param ..., list object
#' @return combined list
#'
#' @export
#'
#' @examples
#'     mylist <- list()
#'     newlist <- push ( 1, mylist )
push <- function(l, ...) c(l, list(...))

#' round_vals
#'
#' Plot PCA results.
#'
#' @param l, the value
#' @return round value
#' @export
#'
#' @examples
#'     x<-round_vals(5.1323223)
round_vals <- function(l) {
    l <- round(as.numeric(l), digits = 2)
    parse(text = l)
}

#' Buttons including Action Buttons and Event Buttons
#'
#' Creates an action button whose value is initially zero, and increments by one
#' each time it is pressed.
#'
#' @param inputId Specifies the input slot that will be used to access the
#'   value.
#' @param label The contents of the button--usually a text label, but you could
#'   also use any other HTML, like an image.
#' @param styleclass The Bootstrap styling class of the button--options are
#'   primary, info, success, warning, danger, inverse, link or blank
#' @param size The size of the button--options are large, small, mini
#' @param block Whehter the button should fill the block
#' @param icon Display an icon for the button
#' @param css.class Any additional CSS class one wishes to add to the action
#'   button
#' @param ... Other argument to feed into shiny::actionButton
#'
#' @export
#'
#' @examples
#'     actionButton("goDE", "Go to DE Analysis!")
#'
actionButton <- function(inputId, label, styleclass = "", size = "",
        block = FALSE, icon = NULL, css.class = "", ...) {
    if (styleclass %in% c("primary", "info", "success", "warning",
        "danger", "inverse", "link")) {
        btn.css.class <- paste("btn", styleclass, sep = "-")
    } else btn.css.class = ""
    
    if (size %in% c("large", "small", "mini")) {
        btn.size.class <- paste("btn", size, sep = "-")
    } else btn.size.class = ""
    
    if (block) {
        btn.block = "btn-block"
    } else btn.block = ""
    
    if (!is.null(icon)) {
        icon.code <- HTML(paste0("<i class='fa fa-", icon, "'></i>"))
    } else icon.code = ""
    tags$button(id = inputId, type = "button", class = paste("btn action-button",
        btn.css.class, btn.size.class, btn.block, css.class, collapse = " "),
        icon.code, label, ...)
}


#' getNormalizedMatrix
#'
#' Normalizes the matrix passed to be used within various methods
#' within DEBrowser.  Requires edgeR package
#'
#' @note \code{getNormalizedMatrix}
#' @param M, numeric matrix
#' @param method, normalization method for edgeR. default is TMM
#' @return normalized matrix
#'
#' @examples
#'     x <- getNormalizedMatrix(mtcars)
#'
#' @export
#'
getNormalizedMatrix <- function(M = NULL, method = "TMM") {
    if (is.null(M) ) return (NULL)
    
    M[is.na(M)] <- 0
    norm <- M
    M <- M[rowSums(M)>0, ]
    if (is.null(M) ) return (NULL)
    if (!(method == "none" || method == "DESeq2")){
        norm.factors <- edgeR::calcNormFactors(M, method = method)
        norm <- edgeR::equalizeLibSizes(edgeR::DGEList(M,
            norm.factors = norm.factors))$pseudo.counts
    }else if(method == "DESeq2"){
        columns <- colnames(M)
        conds <- columns
        coldata <- prepGroup(conds, columns)
        M[, columns] <- apply(M[, columns], 2,
            function(x) as.integer(x))
        dds <- DESeqDataSetFromMatrix(countData = as.matrix(M),
            colData = coldata, design = ~group)
        dds <- estimateSizeFactors(dds)
        norm <- counts(dds, normalized=TRUE)
    }
    return(norm)
}

#' logSliderJScode
#'
#' Generates the log based slider to be used by the user within
#' DEBrowser.
#'
#' @param slidername, id of the slider
#' @note \code{logSliderJScode}
#' @return returns the slider values in log10 scale
#' @examples
#'     x <- logSliderJScode()
#' @export
#'
logSliderJScode <- function(slidername = NULL){
    if (is.null(slidername)) return (NULL)
    paste0("$(function() {
        setTimeout(function(){
        var vals = [0];
        var powStart = 4;
        var powStop = 0;
        for (i = powStart; i >= powStop; i--) {
        var val = Math.pow(10, -i)/2;
        val = parseFloat(val.toFixed(8));
        vals.push(val);
        var val = Math.pow(10, -i);
        val = parseFloat(val.toFixed(8));
        vals.push(val);
        }
        $('#", slidername,"').data('ionRangeSlider').update({'values':vals})
        }, 4)})")
}

#' getCompSelection
#'
#' Gathers the user selected comparison set to be used within the
#' DEBrowser.
#'
#' @param count, comparison count
#' @note \code{getCompSelection}
#' @examples
#'     x <- getCompSelection(count = 2)
#' @export
#'
getCompSelection <- function(count = NULL) {
    a <- NULL
    if (count>1){
        a <- list(selectInput("compselect",
            label = "Choose a comparison:",
            choices = c(1:count) ))
    }
    a
}
#' getHelpButton
#' prepares a helpbutton for to go to a specific site in the documentation
#'
#' @param name, name that are going to come after info
#' @param link, link of the help
#' @return the info button
#'
#' @examples
#'     x<- getHelpButton()
#'
#' @export
getHelpButton<-function(name = NULL, link = NULL){
    if (is.null(name)) return(NULL)
    btn <- actionButton(paste0("info_",name),"",icon="info",
        styleclass="info", size="small")
    
    HTML(paste0("<a id=\"info_",name,"\" href=\"",link,"\" target=\"_blank\">",
       btn,"</a>"))
    
}

#' getDomains
#'
#' Get domains for the main plots.
#'
#' @param filt_data, data to get the domains
#' @return domains
#' @export
#'
#' @examples
#'     x<-getDomains()
getDomains <- function(filt_data = NULL){
    if (is.null(filt_data)) return (NULL)
    a <- unique(filt_data$Legend)
    a <- a[a != ""]
    if (length(a) == 1)
        a <- c(a, "NA")
    a
}

#' getColors
#'
#' get colors for the domains 
#'
#' @param domains, domains to be colored
#' @return colors
#' @export
#'
#' @examples
#'     x<-getColors()
#'
getColors <- function(domains = NULL){
    if (is.null(domains)) return (NULL)
    colors <- c()
    for ( dn in seq(1:length(domains)) ){
        if (domains[dn] == "NS" || domains[dn] == "NA")
            colors <- c(colors, "#aaa")
        else if (domains[dn] == "Up")
            colors <- c(colors, "green")
        else if (domains[dn] == "Down")
            colors <- c(colors, "red")
        else if (domains[dn] == "MV")
            colors <- c(colors, "orange")
        else if (domains[dn] == "GS")
            colors <- c(colors, "blue")
    } 
    colors
}

#' Get the logged in user's email and other info
#' 
#' @param id ID of the person to get the profile data for. 'me' to get current user.
#' 
#' @return A People resource
#' 
#' https://developers.google.com/+/web/api/rest/latest/people#resource-representations
#' 
#' @seealso https://developers.google.com/+/web/api/rest/latest/people
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' options(googleAuthR.scopes.selected = 
#'    c("https://www.googleapis.com/auth/userinfo.email",
#'      "https://www.googleapis.com/auth/userinfo.profile"))
#'                                         
#' googleAuthR::gar_auth()
#' 
#' ## default is user logged in
#' user <- get_user_info()
#' }
#' 
get_user_info <- function(id = "me"){
    
    
    url <- sprintf("https://www.googleapis.com/plus/v1/people/%s", id)
    
    g <- googleAuthR::gar_api_generator(url, "GET")
    
    req <- g()
    
    req$content
    
}

#' Whitelist check
#' 
#' After a user logs in, check to see if they are on a whitelist
#' 
#' @param user_info the object returned by \link{get_user_info}
#' @param whitelist A character vector of emails on whitelist
#' 
#' @return TRUE if on whitelist or no whitelist, FALSE if not
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' options(googleAuthR.scopes.selected = 
#'    c("https://www.googleapis.com/auth/userinfo.email",
#'      "https://www.googleapis.com/auth/userinfo.profile"))
#'                                         
#' googleAuthR::gar_auth()
#' 
#' ## default is user logged in
#' user <- get_user_info()
#' 
#' the_list <- whitelist(user, c("your@email.com", 
#'                               "another@email.com", 
#'                               "yet@anotheremail.com"))
#' 
#' if(the_list){
#'   message("You are on the list.")
#' } else {
#'   message("If you're not on the list, you're not getting in.")
#'}
#' 
#' 
#' 
#' }
whitelist <- function(user_info, whitelist = NULL){
    
    if(user_info$kind != "plus#person"){
        stop("Invalid user object used for user_info")
    }
    
    out <- FALSE
    
    if(is.null(whitelist)){
        message("No whitelist found")
        out <- TRUE
    }
    
    check <- user_info$emails$value
    
    if(is.null(check)){
        stop("No user email found")
    }
    
    if(any(check %in% whitelist)){
        message(check, " is in whitelist ")
        out <- TRUE
    } else {
        message(check, " is NOT on whitelist")
    }
    
    out
    
}