`savews` <-
function (name = .WSID, d = as.character(.UserDate), silentQ = FALSE, 
    historyQ = TRUE) 
{
    if(!TestRWMSetup()) 
       stop("`.UserDirectory` and/or `.UserDate` not set correctly. See help(rwm).")
    if (!exists(".WSID", where=1) && !is.character(name)) 
        stop("`.WSID` not defined!")
    `%<>%` <- function(x,y) paste(x,y,sep="")  
#test if .UserDirectory<>d is valid
    FullUserDirectory <- if (d=="")
        .UserDirectory
    else
       .UserDirectory %<>% "/" %<>% d
    if (!file.exists(FullUserDirectory)) {
        cat(FullUserDirectory %<>% " does not exist, attempting to create it ...", 
            fill = TRUE)
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
    if (exists(".WSID", where=1) && is.same.name(name, .WSID)) 
        SaveName <- name
#case: if .WSID exists and `name` specifies same workspace
    else if (exists(".WSID", where=1)) {
        SaveName <- if (d=="")
            .UserDirectory %<>% "/" %<>% name
        else
            .UserDirectory %<>% "/" %<>% d %<>% "/" %<>% name
#workspace name has changed, check if directory exists
        if (!file.exists(SaveName)) {
            cat(SaveName %<>% " does not exist, attempting to create it...", fill = TRUE)
            dir.create(SaveName, recursive=TRUE)
            if (file.exists(SaveName)) cat(SaveName  %<>%  " created!", fill = TRUE)
            else stop(paste("error creating", SaveName))
        }         
    }
    else {#.WSID does not exist
        SaveName<-paste(.UserDirectory, d, name, sep="/")
         if (!file.exists(SaveName)) {
            cat(SaveName  %<>%  "does not exist, attempting to create it...", fill = TRUE)
            dir.create(SaveName, recursive=TRUE)
            if (file.exists(SaveName)) 
                cat(SaveName  %<>%  " created!", fill = TRUE)
            else stop("error creating "  %<>%  SaveName)
        } 
    }
    base::assign(".WSID", SaveName, envir = .GlobalEnv)
    base::assign(".LastSaved", date(), envir = .GlobalEnv)
    base::setwd(.WSID)   
    base::save.image(".Rdata")
    if (!silentQ) {
        cat("working directory: "  %<>% .WSID,  fill=TRUE)
        cat("saved: "  %<>% .WSID  %<>% "/.Rdata", fill = TRUE)
        }
    if (historyQ) {
        wsRHistory <- SaveName  %<>%  "/.RHistory"
        utils::savehistory(wsRHistory)
        if (!silentQ) 
            cat("saved: "  %<>% wsRHistory, fill = TRUE)
    }
     if (!silentQ) 
        cat(.LastSaved, fill = TRUE)
}

