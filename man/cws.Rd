\name{cws}
\Rdversion{1.1}
\alias{cws}
\title{
Save workspace. Clear and/or quit.
}
\description{
Similar to APL \command{)CONTINUE}, this function can be used to save the workspace 
and quit R.
Or optionally, \code{cws(q=FALSE)}, the workspace is saved, cleared and return to the initial workspace.  
}
\usage{
cws(silentQ = TRUE, q = TRUE)
}

\arguments{
  \item{silentQ}{
No message produced by savews.
}
  \item{q}{
If TRUE, quit R. Otherwise clearws.
}
}

\value{
No value
}

\author{A.I. McLeod }

\seealso{
\code{\link{savews}}, 
\code{\link{clearws}}
}

\examples{
#INITIALIZATION
#Normally .UserDirectory and .UserDate are defined previously. 
#Usually a more convenient directory is used but for illustration
#using a script which will run in interactive or batch mode on any computer:
.UserDirectory <- tempdir()
.UserDate <- "2009"
dir.create(paste(.UserDirectory, .UserDate, sep="/"))

#Simple Example
p<-3
n<-50
e<-rnorm(n)
X<-matrix(rnorm(p*n), ncol=p)
savews("MyWs")
cws(q=FALSE)
}

\keyword{environment}

