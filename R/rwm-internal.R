.onLoad <-
function (libname, pkgname) 
{
    if (testrwm()) {
        setwd(.UserDirectory)
        cat(paste("Current directory:", .UserDirectory), fill = TRUE)
    }
    else cat("Please run `rwmInit()`.", 
        fill = TRUE)
}

