\name{clearws}
\alias{clearws}

\title{Clear workspace}
\description{
Removes all functions and datasets. 
Also removes `.WSID` but not `.UserDirectory`, `.UserDate` and
other hidden variables.
The working directory is set to `.UserDirectory`.
}
\usage{
clearws(silentQ=FALSE)
}

\arguments{
  \item{silentQ}{ FALSE, display message; TRUE, no message }
}

\details{
Using this function is similar to quitting R and starting 
R again with a fresh workspace.
All non-hidden R objects are deleted as well
as the hidden variables `.WSID`, `.UserDate` and `.UserDirectory`.
}
\value{
None
}

\author{A.I. McLeod }

\section{Warning}{
This function can not be called by another function.
}


\seealso{ 
\code{\link{loadws}},
\code{\link{savews}},
 }

\examples{
\dontrun{
loadws(ptest)
ls(all.names=TRUE)
clearws()
ls(all.names=TRUE)
	}
}

\keyword{environment}
