\name{attachws}
\alias{attachws}
\title{ Attach a workspace }

\description{
The named workspace is attached to the search path.
Effectively this makes the workspace functions and data available to
the user in a way similar to R packages.
Documentation is available by using
\code{.Describe} to give a brief description of the contents
or important functions, variables, etc.
}

\usage{
attachws(name = "", d = as.character(.UserDate), prefix = "", pos = 2, LibLocation = "default")
}

\arguments{
  \item{name}{name of workspace. A nonnull argument is required}
  \item{d}{ subdirectory of \code{.UserDirectory}, usually year  }
  \item{prefix}{ if non-null and valid file name, \code{prefix} is prepended to \code{.Rdata} }
  \item{pos}{ position in search path. Must be >= 2 which is the default. }
  \item{LibLocation}{pathname for workspace \code{name} if not default.}
}


\value{
None
}


\author{A.I. McLeod }

\seealso{ 
\code{\link{load}},
\code{\link{savews}}
}

\examples{

#INITIALIZATION
#Normally .UserDirectory and .UserDate are defined previously. 
#Usually a more convenient directory is used but for illustration
#using a script which will run in interactive or batch mode on any computer:
.UserDirectory <- tempdir()
.UserDate <- "2008"
dir.create(paste(.UserDirectory, .UserDate, sep="/"))

#Example 1.  Simple usage example.
#define a new function
is.same.name<-function(x,y) is.logical(all.equal(x,y))&&all.equal(x,y)
#save this workspace as SameName
savews("SameName")  #workspace is in SameName/.Rdata
#save also also with prefix 'My'
savews("SameName", prefix="My") #workspace is in SameName/My.Rdata
#use dir to see both workspaces
dir(all.files=TRUE)
#attach workspace
attachws("SameName", prefix="My")
#check functions in this workspace
objects(2)
#show search path
search()


}


\section{Further Examples}{
Please see vignette.}

\keyword{environment}

