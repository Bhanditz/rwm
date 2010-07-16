loadws <-
function (name="", d = as.character(.UserDate), silentQ = FALSE, 
    historyQ = TRUE, clearQ = TRUE, prefix=.Prefix, pos=1) 
{
if (pos>2)
    base::stop("pos must be set to 1 or 2")
if (pos==2)
    rwm::attachws(name=name, d=d, prefix=prefix, pos=pos)
else {
    UsePath<-FALSE
    HistoryQ <- historyQ
#define catenation 
    `%<>%` <- function(x,y) base::paste(x,y,sep="")   
#test if name is not a character string, eg: myws instead of "myws"
    ans <- base::try(is.character(name),  silent=TRUE) 
    if (!(is.logical(ans) && ans))
        base::stop("Argument, 'name', must be a character string!")
#check prefix
    if (missing(prefix)) 
        Prefix<-""
    else
        Prefix<-prefix
    integerQ <- function(n) base::ceiling(n)==base::floor(n)
    if (base::is.numeric(Prefix) && (Prefix>=0) && integerQ(Prefix))
        Prefix <- as.character(Prefix)
#test if 'prefix' is not a character string, eg: myws instead of "myws"
    ans <- base::try(is.character(Prefix),  silent=TRUE) 
    if (!(base::is.logical(ans) && ans)) {
            base::warning("Argument, 'prefix', must be a character string! Set to NULL")
            Prefix <- ""
            }   
    CopyUserDirectory<-.UserDirectory #keep copy in case it is overwritten by load (this may happen when you switch computers)
    CopyUserDate<-.UserDate 
    #if name is complete workspace name, use it!
    if (file.exists(name)&&!file.info(name)$isdir) {
        j <- nchar(name)
        if (base::tolower(substr(name, j-5, j))==".rdata"){
            WSPathName<-substr(name, 1, j-6)
            s<-WSPathName
            s<-base::strsplit(s,"")[[1]]
            i<-base::rev(grep("/",s))[1]
            j<-base::length(s)
            if (i == j)
                Prefix <- ""
            else 
                Prefix<-base::paste(s[seq(i+1,j)], sep="", collapse="")
            WSPathName<-base::paste(s[seq(1,i-1)], sep="", collapse="")
            UsePathQ <- TRUE
            }
        else 
            base::stop(name, ": Workspace must have extension `.Rdata`.")
        }
    else {
# define WSPathName
        if(!testrwm()) 
            base::stop("`.UserDirectory` and/or `.UserDate` not set correctly. See help(rwm).")
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
    } #end WSPathName now set.
    if (!base::file.exists(WSPathName)) #note WSPathName cannot end in /
            base::stop(base::paste(WSPathName, "does not exist."))
    if (name!=""){
        WSPathNameRdata <- WSPathName %<>% "/" %<>% Prefix %<>% ".Rdata"
        if (!base::file.exists(WSPathNameRdata)) #test workspace exists
            stop(base::paste(WSPathNameRdata, "does not exist."))
        WSPathNameRHistory <- WSPathName %<>% "/" %<>% Prefix %<>% ".RHistory"
        if (!base::file.exists(WSPathNameRHistory)) #test workspace exists
            HistoryQ <- FALSE
        }
   base::setwd(WSPathName)
   if (!silentQ)
        base::cat("working directory: "  %<>% WSPathName, fill=TRUE)
    if (name!=""){
        if (clearQ) {
            base::rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
            if (exists(".LastSaved", where=1, inherits=FALSE)) 
                base::rm(".LastSaved", envir = .GlobalEnv)
            if (exists(".Describe", where=1, inherits=FALSE)) 
                base::rm(".Describe", envir = .GlobalEnv)
            if (exists(".Profile", where=1, inherits=FALSE)) 
                base::rm(".Profile", envir = .GlobalEnv)
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
                base::cat("loaded: " %<>% WSPathName  %<>% "/" %<>% Prefix %<>% ".Rdata", fill=TRUE)
            else
                cat("loaded: " %<>% WSPathName  %<>% "/.Rdata", fill=TRUE)
        if (HistoryQ && interactive()) {
            if (Prefix != "")
                utils::loadhistory(Prefix %<>% ".RHistory")
            else
                utils::loadhistory(".RHistory")
            if (!silentQ)
                if (Prefix != "")
                    base::cat("loaded: " %<>% WSPathName %<>% "/" %<>% Prefix %<>% ".RHistory", fill=TRUE)
                else
                    base::cat("loaded: " %<>% WSPathName %<>% "/.RHistory", fill=TRUE)
        }
        if (!silentQ && exists(".LastSaved", where=1))
            base::cat("last saved: " %<>% .LastSaved, fill = TRUE)
        if (!silentQ && exists(".Describe", where=1)) {
            base::cat(".Describe = ")
            base::cat(.Describe, fill = TRUE)
            }
#It is unlikely that objects 'loadws', 'savews', etc. exist
#  in the workspace being loaded but if they do, it's a catastrophe,
#  the following checks are needed. 
        if (exists("loadws", where=1, inherits=FALSE)){
            base::cat("'loadws' exists in current workspace. Removed.", fill=TRUE)
            base::rm("loadws", pos=1)
            } 
        if (exists("attachws", where=1, inherits=FALSE)){
            base::cat("'attachws' exists in current workspace. Removed.", fill=TRUE)
            base::rm("attachws", pos=1)
            } 
        if (exists("savews", where=1, inherits=FALSE)){
            base::cat("'savews' exists in current workspace. Removed.", fill=TRUE)
            base::rm("savews", pos=1)
            }
        if (exists("clearws", where=1, inherits=FALSE)){
            base::cat("'clearws' exists in current workspace. Removed.", fill=TRUE)
            base::rm("clearws", pos=1)
            }
        if (exists("cws", where=1, inherits=FALSE)){
            base::cat("'cws' exists in current workspace. Removed.", fill=TRUE)
            base::rm("cws", pos=1)
            }                   
    }
}
}
