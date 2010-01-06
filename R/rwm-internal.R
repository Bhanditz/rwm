.First.lib <-
function (lib, pkg) 
{
    if (testrwm()) {
        setwd(.UserDirectory)
        cat(paste("Current directory:", .UserDirectory), fill = TRUE)
    }
    else cat("Please run `initrwm()`. Then quit R and reload it", 
        fill = TRUE)
}

