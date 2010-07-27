savews <-
function (name = .WSID, d = as.character(.UserDate), silentQ = FALSE, 
    historyQ = TRUE, q = FALSE, prefix=.Prefix) 
{
    `%<>%` <- function(x,y) paste(x,y,sep="")  
    if (file.exists(name)){
        WSPathName<-name
        Prefix <- ""
        }
    else{
        if(!testrwm()) 
            stop("`.UserDirectory` and/or `.UserDate` not set correctly. See help(rwm).")
        if (!exists(".WSID", where=1, inherits=FALSE) && !is.character(name)) 
            stop("`.WSID` not defined!\nArgument name must be character string if used.\nOtherwise if argument `name` is not used, `.WSID` must be a valid pathanme with read/write privledges.")
        if (missing(prefix)&&!exists(".Prefix", where=1, inherits=FALSE) ) 
            Prefix<-""
        else
            Prefix<-prefix
        integerQ <- function(n) ceiling(n)==floor(n)
        if (is.numeric(Prefix) && (Prefix>=0) && integerQ(Prefix))
            Prefix <- as.character(Prefix)        
#test if 'prefix' is not a character string, eg: myws instead of "myws"
        ans <- base::try(is.character(Prefix),  silent=TRUE) 
        if (!(is.logical(ans) && ans))
            base::stop("Argument, 'prefix', must be a character string!")
#test if .UserDirectory<>d is valid
        if (d=="")
            FullUserDirectory <- .UserDirectory
        else
            FullUserDirectory <- .UserDirectory %<>% "/" %<>% d
        if (!file.exists(FullUserDirectory)) {
            dir.create(FullUserDirectory, recursive=TRUE)
        if (file.exists(FullUserDirectory))
            cat(FullUserDirectory %<>% " created!", fill = TRUE)
        else stop("error creating" %<>% FullUserDirectory)
        }
#name must be a character string or missing
        ans <- try(is.character(name),  silent=TRUE)
        if (!(is.logical(ans) && ans))
            stop("First argument must be a character string")
        is.same.name <- function(x, y) is.logical(all.equal(x, y)) && 
            all.equal(x, y)
#case: name=.WSID
        if (exists(".WSID", where=1, inherits=FALSE) && is.same.name(name, .WSID)) 
            WSPathName <- name
#case: if .WSID exists and `name` specifies same workspace
        else if (exists(".WSID", where=1, inherits=FALSE)) {
            WSPathName <- if (d=="")
                .UserDirectory %<>% "/" %<>% name
            else
                .UserDirectory %<>% "/" %<>% d %<>% "/" %<>% name
#workspace name has changed, check if directory exists
            if (!file.exists(WSPathName)) {
                dir.create(WSPathName, recursive=TRUE)
                if (file.exists(WSPathName)) cat(WSPathName  %<>%  " created!", fill = TRUE)
                else stop(paste("error creating", WSPathName))
            }         
        }
        else {#.WSID does not exist
            WSPathName<-paste(.UserDirectory, d, name, sep="/")
            if (!file.exists(WSPathName)) {
                dir.create(WSPathName, recursive=TRUE)
                if (file.exists(WSPathName)) 
                    cat(WSPathName  %<>%  " created!", fill = TRUE)
                else stop("error creating "  %<>%  WSPathName)
            } 
        }
    }
    base::assign(".WSID", WSPathName, envir = .GlobalEnv)
    if (Prefix!="")
        base::assign(".Prefix", Prefix, envir = .GlobalEnv)
    base::assign(".LastSaved", date(), envir = .GlobalEnv)
    base::setwd(.WSID)
    WSName <- paste(Prefix, ".Rdata", sep="")
    HistoryName <- paste(Prefix, ".RHistory", sep="")
    ok <- file.create(WSName, showWarnings=FALSE)
    if (!ok) {
      warning("invalid, prefix =", prefix)
      cat("reset to default, NULL",fill=TRUE)
      WSName <- ".Rdata"  #setting to defaults
      HistoryName <- ".RHistory"
    }  
    base::save.image(WSName)
    if (!silentQ) {
        cat("working directory: "  %<>% .WSID,  fill=TRUE)
        cat("saved: "  %<>% .WSID  %<>% "/" %<>% WSName, fill = TRUE)
        }
    if (historyQ&&interactive()) {
        wsRHistory <- WSPathName  %<>%  "/" %<>% HistoryName
        utils::savehistory(wsRHistory)
        if (!silentQ) 
            cat("saved: "  %<>% wsRHistory, fill = TRUE)
    }
     if (!silentQ) 
        cat(.LastSaved, fill = TRUE)
     if (q) q("no")
}
