`TestRWMSetup` <-
function() {
    if (exists(".UserDirectory", where=1, inherits=FALSE) && file.exists(.UserDirectory) && exists(".UserDate", where=1, inherits=FALSE)) 
        if (.UserDate=="")
                TRUE
        else
            file.exists(base::paste(.UserDirectory, .UserDate, sep="/"))
    else
        FALSE
}

