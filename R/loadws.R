loadws <-
function (name="", d = as.character(.UserDate), silentQ = FALSE, 
    historyQ = TRUE, clearQ = TRUE, prefix=.Prefix, pos=1) 
{
if (pos>2)
	stop("pos must be set to 1 or 2")
if (pos==2)
    attachws(name=name, d=d, prefix=prefix, pos=pos)
else {
    if(!testrwm()) 
       base::stop("`.UserDirectory` and/or `.UserDate` not set correctly. See help(rwm).")
    CopyUserDirectory<-.UserDirectory #keep copy in case it is overwritten by load (this may happen when you switch computers)
    CopyUserDate<-.UserDate
    HistoryQ <- historyQ
    #test if name is not a character string, eg: myws instead of "myws"
    ans <- try(is.character(name),  silent=TRUE) 
    if (!(is.logical(ans) && ans))
        base::stop("Argument, 'name', must be a character string!") 
    if (missing(prefix)) 
        Prefix<-""
    else
        Prefix<-prefix
    #test if 'prefix' is not a character string, eg: myws instead of "myws"
    ans <- base::try(is.character(Prefix),  silent=TRUE) 
    if (!(is.logical(ans) && ans)) {
        base::warning("Argument, 'prefix', must be a character string! Set to NULL")
        Prefix <- ""
        }
#define catenation 
    `%<>%` <- function(x,y) base::paste(x,y,sep="")   
    WSPathName <- if (name=="")
        .UserDirectory %<>% "/" %<>% d
    else 
        .UserDirectory %<>% "/" %<>% d %<>% "/" %<>% name
    #replace possible \\ with /
    WSPathName <- base::sub("\\\\", "/", WSPathName)
    rtb<-function(x) { #replace any possible trailing blackslash
        if (base::substr(x, start=nchar(x), stop=nchar(x))=="/")
            base::substr(x, start=1, stop=nchar(x)-1)
        else   x
    }
    #both workspace directory and workspace must exist
    #  or error is given
    WSPathName <- rtb(WSPathName)
    if (!file.exists(WSPathName)) #note WSPathName cannot end in /
            stop(paste(WSPathName, "does not exist."))
    if (name!=""){
        WSPathNameRdata <- WSPathName %<>% "/" %<>% Prefix %<>% ".Rdata"
        if (!file.exists(WSPathNameRdata)) #test workspace exists
            stop(paste(WSPathNameRdata, "does not exist."))
        WSPathNameRHistory <- WSPathName %<>% "/" %<>% Prefix %<>% ".RHistory"
        if (!file.exists(WSPathNameRHistory)) #test workspace exists
            HistoryQ <- FALSE
        }
   setwd(WSPathName)
   if (!silentQ)
        cat("working directory: "  %<>% WSPathName, fill=TRUE)
    if (name!=""){
        if (clearQ) {
            rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
            if (exists(".LastSaved", where=1, inherits=FALSE)) 
                rm(".LastSaved", envir = .GlobalEnv)
            if (exists(".Describe", where=1, inherits=FALSE)) 
                rm(".Describe", envir = .GlobalEnv)
            if (exists(".Profile", where=1, inherits=FALSE)) 
                rm(".Profile", envir = .GlobalEnv)
        }
        if (Prefix != "")
            base::load(Prefix %<>% ".Rdata", .GlobalEnv)
        else
            base::load(".Rdata", .GlobalEnv)
        base::assign(".WSID", WSPathName, envir = .GlobalEnv)
        base::assign(".Prefix", Prefix, envir = .GlobalEnv)
        base::assign(".UserDirectory", CopyUserDirectory, envir = .GlobalEnv)
        base::assign(".UserDate", CopyUserDate, envir = .GlobalEnv)
        if (!silentQ)
            if (Prefix != "") 
                cat("loaded: " %<>% WSPathName  %<>% "/" %<>% Prefix %<>% ".Rdata", fill=TRUE)
            else
                cat("loaded: " %<>% WSPathName  %<>% "/.Rdata", fill=TRUE)
        if (HistoryQ && interactive()) {
            if (Prefix != "")
                utils::loadhistory(Prefix %<>% ".RHistory")
            else
                utils::loadhistory(".RHistory")
            if (!silentQ)
                if (Prefix != "")
                    cat("loaded: " %<>% WSPathName %<>% "/" %<>% Prefix %<>% ".RHistory", fill=TRUE)
                else
                    cat("loaded: " %<>% WSPathName %<>% "/.RHistory", fill=TRUE)
        }
        if (!silentQ && exists(".LastSaved", where=1))
            cat("last saved: " %<>% .LastSaved, fill = TRUE)
        if (!silentQ && exists(".Describe", where=1)) {
            cat(".Describe = ")
            cat(.Describe, fill = TRUE)
            }
#It is unlikely that objects 'loadws', 'savews', etc. exist
#  in the workspace being loaded but if they do, it's a catastrophe,
#  the following checks are needed. 
        if (exists("loadws", where=1, inherits=FALSE)){
            cat("'loadws' exists in current workspace. Removed.", fill=TRUE)
            base::rm("loadws", pos=1)
            } 
        if (exists("attachws", where=1, inherits=FALSE)){
            cat("'attachws' exists in current workspace. Removed.", fill=TRUE)
            base::rm("attachws", pos=1)
            } 
        if (exists("savews", where=1, inherits=FALSE)){
            cat("'savews' exists in current workspace. Removed.", fill=TRUE)
            base::rm("savews", pos=1)
            }
        if (exists("clearws", where=1, inherits=FALSE)){
            cat("'clearws' exists in current workspace. Removed.", fill=TRUE)
            base::rm("clearws", pos=1)
            }
        if (exists("cws", where=1, inherits=FALSE)){
            cat("'cws' exists in current workspace. Removed.", fill=TRUE)
            base::rm("continuews", pos=1)
            }                   
    }
}
}

