cws <-
function(silentQ=TRUE, q=TRUE){
savews(silentQ=silentQ)
if (q)
    q("no")
else
    clearws()
}

