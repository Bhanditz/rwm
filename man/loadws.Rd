\name{loadws}
\alias{loadws}

\title{ load workspace }
\description{
Loads R workspace and history. Move to workspace directory.
}
\usage{
loadws(name = "", d = as.character(.UserDate), silentQ = FALSE, historyQ = TRUE, clearQ = TRUE)
}

\details{
This simply uses `load(...)` and `loadhistory(...)` 
to load the R workspace and command history specified by the arguments.
Normally all R objects are removed using 'clearws(silent=TRUE)' before loading the
workspace.

The working directory is set the the workspace working directory unless
`name` is the null string. 
In this case, the current directory is set to `.UserDirectory`<>`d`.

An error message is given if `.UserDirectory` is not defined.
The `.UserDirectory` may be set manually using a normal
R assignment or by the function InitializeRWM().
}

\arguments{
  \item{name}{name of workspace. If "", move to directory specified
  by .UserDirectory<>d  }
  \item{d}{ subdirectory of .UserDirectory, usually year  }
  \item{silentQ}{ TRUE, no message }
  \item{historyQ}{ TRUE, save R history }
  \item{clearQ}{ TRUE, clear all R objects before loading the workspace }
}

\value{
The global variable `.WSID` is set to the current working directory.
}

\references{ 
McLeod, A.I. (2008). R Workspace Management: With R Package.
}

\author{A.I. McLeod }

\note{
To save this workspace back where it was use `savews()`.

If the workspace contains a character vector `.Describe`, this is also
displayed when the workspace is loaded. 
`.Describe` may contain a brief description of the purpose of the workspace and/or major
functions/datasets in the workspace.
}

\seealso{ 
\code{\link{load}},
\code{\link{savews}},
\code{\link{clearws}},
\code{\link{InitializeRWM}}
}

\examples{
\dontrun{
#Example 1. 
#Test if setup is ok
TestRWMSetup()
#define a new function
is.same.name<-function(x,y) is.logical(all.equal(x,y))&&all.equal(x,y)
#save this workspace as SameName
savews("SameName")
#check .WSID
.WSID
#also save R source file in workspace directory
dump("is.name.same", file="is.name.same.R")
#see all objects in directory
dir()
}

}

\keyword{environment}


