\name{continuews}
\alias{continuews}

\title{ Save workspace and quit R }
\description{
This implements the APL command )CONTINUE.
}
\usage{
continuews(silentQ = TRUE)
}

\arguments{
  \item{silentQ}{ TRUE, no messages.}
}

\details{
Simply  'savews` and `q`.
An error is given if `.WSID` is not defined.
}
\value{
None
}

\references{ 
McLeod, A.I. (2008). R Workspace Management: With R Package.
}

\author{A.I. McLeod }


\seealso{ 
\code{\link{savews}} }

\examples{

\dontrun{
#load some workspace
loadws(ptest)
#...make some changes....
continuews() #quits R and exits
}

}
\keyword{environment}
