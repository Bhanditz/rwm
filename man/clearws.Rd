\name{clearws}
\alias{clearws}

\title{Clear workspace}
\description{
Removes all functions and datasets. 
Also removes \code{.WSID} but not \code{.UserDirectory}, \code{.UserDate} and
other hidden variables.
The working directory is reset to \code{.UserDirectory}.
}
\usage{
clearws(silentQ=FALSE)
}

\arguments{
  \item{silentQ}{ FALSE, display message; TRUE, no message }
}

\details{
Using this function is similar to quitting R and starting 
R again with a fresh workspace except that the
R command history is kept.
All non-hidden R objects are deleted as well
as the hidden variables \code{.WSID}, \code{.LastSaved} and  \code{.Describe}
if they exist in the current workspace.
}

\value{
None
}

\author{A.I. McLeod }

\seealso{ 
\code{\link{loadws}},
\code{\link{savews}},
 }

\examples{
#INITIALIZATION
#Normally .UserDirectory and .UserDate are defined previously. 
#Usually a more convenient directory is used but for illustration
#using a script which will run in interactive or batch mode on any computer:
.UserDirectory <- tempdir()
.UserDate <- "2008"
dir.create(paste(.UserDirectory, .UserDate, sep="/"))

#Fit model to airquality data, save and clear
NYO3.fit <- lm(Ozone~Solar.R + Wind + Temp, data=airquality)
NYO3.fit 
savews("NYO3")
clearws()
ls(all.names=TRUE)
}

\keyword{environment}
