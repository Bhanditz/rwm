\name{savews}
\alias{savews}

\title{ Save workspace }
\description{
Save R workspace and history.
}
\usage{
savews(name = .WSID, d = as.character(.UserDate), silentQ = FALSE, historyQ = TRUE)
}

\arguments{
  \item{name}{name of workspace  }
  \item{d}{ usually year but could be any other part  }
  \item{silentQ}{ TRUE, no message}
  \item{historyQ}{ TRUE, save R history }
}

\value{
Just before saving, the following is done:
(i) the global variable `.WSID` is set to the working directory for the saved workspace
and (ii) the global variable `.UserDate` is set.
}

\details{
First the working directory is changed to that specified by the arguments.
The working directory is specified by concatenating `.UserDirectory`<>`d`<>`name` unless
`name` is set to its default `.WSID`. 
In this case the working directory is simply `.WSID`. 
Then `save.image(".Rdata")`  and `savehistory(".Rhistory")` are used.
The global variable `.WSID` is set to the working directory.

An error message is given if `.UserDirectory` is not defined.
The `.UserDirectory` may be set manually using a normal
R assignment or by the function `IntializeRWM()`.
}


\references{ 
McLeod, A.I. (2008). R Workspace Management: With R Package.
}

\author{A.I. McLeod }

\note{ `savews()` is a useful shortcut. It saves the current workspace
in exactly the same file it was in when loaded. }

\seealso{ 
\code{\link{load}},
\code{\link{savews}},
\code{\link{clearws}},
\code{\link{InitializeRWM}}
}

\examples{
\dontrun{
#Example 1.
#load a previously saved workspaced in current year
savews("myws")

#Example 2.
#load a previously saved workspaced from 2005
savews("ktest", d=2005)
#...make some changes... and then re-save
savews()
}

}

\keyword{environment}



