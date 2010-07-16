attachws <-
function (name="", d = as.character(.UserDate), prefix=.Prefix, pos=2, LibLocation="default") 
{
    #test if 'name' is not a character string, eg: myws instead of "myws"
    ans <- base::try(is.character(name),  silent=TRUE) 
    if (!(is.logical(ans) && ans))
        stop("Argument, 'name', must be a character string!") 
    #if name is complete workspace name, use it!
    if (file.exists(name)&&!file.info(name)$isdir) {
        j <- nchar(name)
        if (base::tolower(substr(name, j-5, j))==".rdata"){
            newws<-name
        }
        else 
            base::stop(name, "is not valid")
    }
    else {
        if (LibLocation=="default"&&!testrwm()) 
        base::stop("`.UserDirectory` and/or `.UserDate` not set correctly. See help(rwm).")
#test if 'prefix' is not a character string, eg: myws instead of "myws"
        Prefix <- ""
        if (!missing(prefix) && prefix != "")
            Prefix <- prefix
        integerQ <- function(n) ceiling(n)==floor(n)
        if (is.numeric(Prefix) && (Prefix>=0) && integerQ(Prefix))
            Prefix <- as.character(Prefix)
        ans <- base::try(is.character(Prefix),  silent=TRUE) 
        if (!(base::is.logical(ans) && ans))
            stop("Argument, 'prefix', must be a character string!")
        `%<>%` <- function(x,y) base::paste(x,y,sep="")
        if (LibLocation=="default")    
            UD <- .UserDirectory %<>% "/" %<>% d %<>% "/" %<>% name
        else
            UD <- LibLocation %<>% "/" %<>% d %<>% "/" %<>% name
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
        if (!base::file.exists(UD)) #note UD cannot end in /
            base::stop(paste(UD, ": Workspace does not exist."))
        newws <- UD %<>% "/" %<>% Prefix %<>% ".Rdata"
    }
    base::attach(newws, pos, warn.conflicts=FALSE)
    cat("attached: ", newws, fill=TRUE)
    if (base::exists(".Describe", where=pos)) {
            base::cat(".Describe = ")
            base::cat(.Describe, fill = TRUE)
    }
}
