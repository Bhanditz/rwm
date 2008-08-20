`.First.lib` <-
function (lib, pkg) 
{
    if (TestRWMSetup()) {
        setwd(.UserDirectory)
        cat(paste("Current directory:", .UserDirectory), fill = TRUE)
    }
    else cat("Please run `InitializeRWM()`. Then quit R and reload it", 
        fill = TRUE)
}
