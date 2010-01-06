testrwm <-
function() {
if(exists(".UserDirectory", where=1, inherits=FALSE) && 
   file.exists(.UserDirectory) && 
   exists(".UserDate", where=1, inherits=FALSE)) 
   if (.UserDate=="")
       ans <- TRUE
   else {
        UD<-base::paste(.UserDirectory, .UserDate, sep="/")
        if (file.exists(UD) || dir.create(UD))
             ans <- TRUE
         }
            else
                ans <- FALSE
        ans
}

