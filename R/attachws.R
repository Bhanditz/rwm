attachws <-
function (name="", d = as.character(.UserDate), prefix="", pos=2) 
{
    if(!testrwm()) 
       stop("`.UserDirectory` and/or `.UserDate` not set correctly. See help(rwm).")
    CopyUserDirectory<-.UserDirectory #keep copy in case it is overwritten by load (this may happen when you switch computers)
    CopyUserDate<-.UserDate
    #test if 'name' is not a character string, eg: myws instead of "myws"
    ans <- try(is.character(name),  silent=TRUE) 
    if (!(is.logical(ans) && ans))
        stop("Argument, 'name', must be a character string!") 
    #test if 'prefix' is not a character string, eg: myws instead of "myws"
    ans <- try(is.character(prefix),  silent=TRUE) 
    if (!(is.logical(ans) && ans))
        stop("Argument, 'prefix', must be a character string!")
    Prefix <- NULL
    if (prefix != "")
        Prefix <- prefix 
    `%<>%` <- function(x,y) base::paste(x,y,sep="")   
    UD <- .UserDirectory %<>% "/" %<>% d %<>% "/" %<>% name
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
    base::attach(Prefix %<>% ".Rdata", pos, warn.conflicts=FALSE)
    cat("attached: " %<>% UD  %<>% Prefix %<>% "Rdata", fill=TRUE)
    if (exists(".Describe", where=pos)) {
            cat(".Describe = ")
            cat(.Describe, fill = TRUE)
    }
}

