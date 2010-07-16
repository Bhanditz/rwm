testrwm <-
function() {
if(base::exists(".UserDirectory", where=1, inherits=FALSE) && 
   base::file.exists(.UserDirectory) && 
   base::exists(".UserDate", where=1, inherits=FALSE)) 
   if (.UserDate=="")
       ans <- TRUE
   else {
        UD<-base::paste(.UserDirectory, .UserDate, sep="/")
        if (base::file.exists(UD) || base::dir.create(UD))
             ans <- TRUE
         }
            else
                ans <- FALSE
        ans
}
