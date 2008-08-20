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
        stop("First argument must be a character string") 
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
            if (exists(".LastSaved", where=1)) 
                rm(".LastSaved", envir = .GlobalEnv)
            if (exists(".Describe", where=1)) 
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
        if (!silentQ && exists(".Describe", where=1))
            cat(.Describe, fill = TRUE)                
    }
}

