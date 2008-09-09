`clearws` <-
function (silentQ = FALSE) 
{
    if (!TestRWMSetup()) 
        stop("`.UserDirectory` or `.UserDate` not defined. See help(rwm).")
    setwd(.UserDirectory)
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    if (exists(".WSID", where=1, inherits=FALSE)) 
        rm(".WSID", envir = .GlobalEnv)
    if (exists(".LastSaved", where=1, inherits=FALSE)) 
        rm(".LastSaved", envir = .GlobalEnv)
    if (exists(".Describe", where=1, inherits=FALSE)) 
        rm(".Describe", envir = .GlobalEnv)
    if (!silentQ) {
        cat("Workspace cleared.", fill = TRUE)
        cat(paste("Working directory:", .UserDirectory), fill = TRUE)
    }
}

