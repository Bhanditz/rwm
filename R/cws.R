cws <-
function(silentQ=TRUE, q=TRUE){
rwm::savews(silentQ=silentQ)
if (q)
    base::q("no")
else
    rwm::clearws()
}

