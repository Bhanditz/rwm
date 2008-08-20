`InitializeRWM` <-
function(){
    `%<>%` <- function(x,y) paste(x,y,sep="")
    rtb<-function(x) { #replace any possible trailing blackslash
        if (base::substr(x, start=nchar(x), stop=nchar(x))=="/")
            ans<-base::substr(x, start=1, stop=nchar(x)-1)
        else    ans<-x
    ans
    }
#setting `.UserDirectory`
if (!exists(".UserDirectory", where=1)) {
    cat("The overall directory is where all your R projects will be saved.\nEach R project may be comprised of further subdirectories.\nThese directories and subdirectories may contain\nworkspaces, R source files and other types of data as well.\nFor example, on windows, I use d:/r/")
    ans <- readline("\nEnter the overall directory for your R projects:\n")
# possible minor editorial adjustments
                       #1. replace possible \\ with /
    ans <- base::gsub("//","/",gsub("(\\\\)", "/", ans))
    ans <- rtb(ans)
    if (!file.exists(ans)){
        cat(ans %<>% " does not exist!", fill=TRUE)
        ansyn<-readline("Would you like to create it?")
        if (substr(ansyn, 1, 1)=="y" || substr(ansyn, 1, 1)=="Y"){
            dir.create(ans)
            cat(ans %<>% " created!", fill=TRUE)
        }
    else 
        stop("You will need to create a suitable directory. Please see help(rwm).", call=FALSE)
     }
    .UserDirectory<<-ans
    cat("`.UserDirectory` set to: " %<>% ans, fill=TRUE)
}
##
if (!exists(".UserDate", where=1)) {
    cat(paste("Within ",.UserDirectory,", projects may be organized by year or other grouping",sep=""),fill=TRUE)
    cat("Set the variable `.UserDate` to current year or other value", fill=TRUE)
    cat("For example .UserDate<-2008 or .UserDate<-\"current\" or .UserDate<-\"A\" ", fill=TRUE)
    cat("Or simply press `Enter` if you don't want to use it.", fill=TRUE)
    ans <- readline("Enter .UserDate: \n")
    pathName <-.UserDirectory %<>% "/" %<>% ans
    pathName <- gsub("//","/",gsub("(\\\\)", "/", pathName))
    pathName <- rtb(pathName)
    if (!file.exists(pathName)){
        cat(pathName %<>% " does not exist!", fill=TRUE)
        ansyn<-readline("\nWould you like to create it?\n")
        if (substr(ansyn, 1, 1)=="y" || substr(ansyn, 1, 1)=="Y"){
            dir.create(pathName)
            cat(pathName %<>% " created!", fill=TRUE)
        }
        else 
        stop("You will need to create a suitable directory. Then run InitializeRWM().",fill=TRUE) 
        }
    .UserDate<<-ans
    cat("`.UserDate` set to " %<>% ans, fill=TRUE)
}
#At this point we know that `.UserDirectory` and `.UserDate` are set, 
#   now check if `.UserDirectory`<>`.UserDate` is a valid directory.
if (TestRWMSetup()) {
#setup ok, now save workspace
    cat("`.UserDirectory` and `.UserDate` are set correctly",fill=TRUE)
    save.image()
    cat("workspace saved: "  %<>% getwd()  %<>% "/.Rdata", fill=TRUE)
    cat("This should be used as your startup workspace.",fill=TRUE)
    }
else #error in setup
    if (.UserDate=="")
        stop(.UserDirectory  %<>% " does not exist!", call=FALSE)
    else
        stop(.UserDirectory  %<>% " or " %<>% .UserDate %<>% " does not exist!", call=FALSE)
}

