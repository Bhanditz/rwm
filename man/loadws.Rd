\name{loadws}
\alias{loadws}

\title{ load workspace }
\description{
Loads R workspace and history. Move to workspace directory.
}
\usage{loadws(name = "", d = as.character(.UserDate), silentQ = FALSE, 
historyQ = TRUE, clearQ = TRUE, prefix = .Prefix, pos=1)
}

\details{
This simply uses the R base functions \code{load} and \code{loadhistory} 
to load the R workspace and command history specified by the arguments.
Normally all R objects are removed using \code{clearws(silent=TRUE)} before loading the
workspace.

The working directory is set the the workspace working directory unless
\code{name} is the null string. 
In this case, the current directory is set to \code{.UserDirectory}\code{<>}\code{d}.

An error message is given if \code{.UserDirectory} is not defined.
The \code{.UserDirectory} may be set manually using a normal
R assignment or by the function irwm().

If the workspace contains a character vector \code{.Describe}, this is also
displayed when the workspace is loaded. 
\code{.Describe} may contain a brief description of the purpose of the workspace and/or major
functions/datasets in the workspace.

}

\arguments{
  \item{name}{name of workspace to load and move to or if \code{name = ""}, move to directory specified
  by \code{.UserDirectory <> d}  }
  \item{d}{ an optional subdirectory of .UserDirectory, usually set to year  }
  \item{silentQ}{ TRUE, no message }
  \item{historyQ}{ TRUE, load R history }
  \item{clearQ}{ TRUE, clear all R objects before loading the workspace }
  \item{prefix}{ If non-null and valid file name, \code{prefix} is prepended to \code{.Rdata}. 
  If .Prefix is undefined, NULL prefix is used. }
  \item{pos}{ position in search path. When pos=1, the workspace becomes the global environment
  or root workspace. For pos=2, the workspace is simply attached at position 2 using
  the \code{attachws}. And pos>2, results in an error.}
}

\value{
The global variable \code{.WSID} is set to the current working directory.
}

\author{A.I. McLeod }

\section{Warning}{
In the unlikely event that the workspace being loaded already happens to contain
one of the objects \code{loadws}, \code{savews}, \code{clearws} or  \code{cws} 
a warning will be given and the object will be deleted.
}

\seealso{ 
\code{\link{attachws}},
\code{\link{load}},
\code{\link{savews}},
\code{\link{clearws}},
\code{\link{rwmInit}}
}

\examples{
#INITIALIZATION
#Normally .UserDirectory and .UserDate are defined previously. 
#Usually a more convenient directory is used but for illustration
#using a script which will run in interactive or batch mode on any computer:
.UserDirectory <- tempdir()
.UserDate <- "2009"
dir.create(paste(.UserDirectory, .UserDate, sep="/"))

#Example 1. 
#define a new function
is.same.name<-function(x,y) is.logical(all.equal(x,y))&&all.equal(x,y)
#save this workspace as SameName
savews("SameName")
#check .WSID
.WSID
#also save R source file in workspace directory
dump("is.name.same", file="is.name.same.R")
#see all objects in directory
dir(all.files=TRUE)
#Use a prefix with savews
savews("SameName", prefix="MyWorkspace")
dir(all.files=TRUE)

}

\keyword{environment}


