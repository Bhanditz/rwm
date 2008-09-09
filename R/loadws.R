`loadws` <-
function (name="", d = as.character(.UserDate), silentQ = FALSE, 
    historyQ = TRUE, clearQ = TRUE) 
{
    if(!TestRWMSetup()) 
       stop("`.UserDirectory` and/or `.UserDate` not set correctly. See help(rwm).")
    CopyUserDirectory<-.UserDirectory #keep copy in case it is overwritten by load (this may happen when you switch computers)
    CopyUserDate<-.UserDate
    #test if name is not a character string, eg: myws instead of "myws"
    ans <- try(is.character(name),  silent=TRUE) 
    if (!(is.logical(ans) && ans))
        stop("Argument, 'name', must be a character string!") 
    `%<>%` <- function(x,y) base::paste(x,y,sep="")   
    UD <- if (name=="")
        .UserDirectory %<>% "/" %<>% d
    else 
        .UserDirectory %<>% "/" %<>% d %<>% "/" %<>% name
    #replace possible \\ with /
    UD <- base::sub("\\\\", "/", UD)
    rtb<-function(x) { #replace any possible trailing blackslash
        if (base::substr(x, start=nchar(x), stop=nchar(x))=="/")
            base::substr(x, start=1, stop=nchar(x)-1)
        else   x
    }
    #both workspace directory and workspace must exist
    #  or error is given
    UD <- rtb(UD)
    if (!file.exists(UD)) #note UD cannot end in /
            stop(paste(UD, "does not exist."))
    if (name!=""){
        UDRdata <- UD %<>% "/.Rdata"
        if (!file.exists(UDRdata)) #test workspace exists
            stop(paste(UDRdata, "does not exist."))
        }
    setwd(UD)
   if (!silentQ)
        cat("working directory: "  %<>% UD, fill=TRUE)
    if (name!=""){
        if (clearQ) {
            rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
            if (exists(".LastSaved", where=1, inherits=FALSE)) 
                rm(".LastSaved", envir = .GlobalEnv)
            if (exists(".Describe", where=1, inherits=FALSE)) 
                rm(".Describe", envir = .GlobalEnv)
        }
        base::load(".Rdata", .GlobalEnv)
        assign(".WSID", UD, envir = .GlobalEnv)
        assign(".UserDirectory", CopyUserDirectory, envir = .GlobalEnv)
        assign(".UserDate", CopyUserDate, envir = .GlobalEnv)
        if (!silentQ) 
            cat("loaded: " %<>% UD  %<>% "/.Rdata", fill=TRUE)
        if (historyQ) {
            utils::loadhistory(".RHistory")
            if (!silentQ)
                cat("loaded: " %<>% UD  %<>% "/.RHistory", fill=TRUE)
        }
        if (!silentQ && exists(".LastSaved", where=1))
            cat("last saved: " %<>% .LastSaved, fill = TRUE)
        if (!silentQ && exists(".Describe", where=1)) {
            cat(".Describe = ")
            cat(.Describe, fill = TRUE)
            }
#It is unlikely that objects 'loadws', 'savews', 'clearws' or 'continuews' exist
#  in the workspace being loaded but if they do, it's a catastrophe, so I feel
#  the following checks are needed. 
        if (exists("loadws", where=1, inherits=FALSE)){
            cat("'loadws' exists in current workspace.", fill=TRUE)
            ans<-readline("remove 'loadws' from current workspace (recommended): y/n ")
            if (substr(ans, 1, 1) == "n")
                stop("error - `loadws` exists in current workspace!\nRemove this conflict to use `rwm`!")
            else
                rm("loadws", pos=1)
            } 
        if (exists("savews", where=1, inherits=FALSE)){
            cat("'savews' exists in current workspace.", fill=TRUE)
            ans<-readline("remove 'savews' from current workspace (recommended): y/n ")
            if (substr(ans, 1, 1) == "n")
                stop("error - `savews` exists in current workspace!\nRemove this conflict to use `rwm`!")
            else
                rm("savews", pos=1)
            }
        if (exists("clearws", where=1, inherits=FALSE)){
            cat("'clearws' exists in current workspace.", fill=TRUE)
            ans<-readline("remove 'clearws' from current workspace (recommended): y/n ")
            if (substr(ans, 1, 1) == "n")
                stop("error - `clearws` exists in current workspace!\nRemove this conflict to use `rwm`!")
            else
                rm("clearws", pos=1)
            }
        if (exists("continuews", where=1, inherits=FALSE)){
            cat("'continuews' exists in current workspace.", fill=TRUE)
            ans<-readline("remove 'continuews' from current workspace (recommended): y/n ")
            if (substr(ans, 1, 1) == "n")
                stop("error - `continuews` exists in current workspace!\nRemove this conflict to use `rwm`!")
            else
                rm("continuews", pos=1)
            }                     
    }
}

