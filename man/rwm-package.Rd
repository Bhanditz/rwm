\name{rwm-package}
\alias{rwm-package}
\alias{rwm}
\docType{package}
\title{R Workspace Manager}

\description{
Utility Functions for Managing R Workspaces
}

\details{
	\tabular{ll}{
	Package: \tab rwm\cr
	Type: \tab Package\cr
	Version: \tab 1.27\cr
	Date: \tab 2008-09-09\cr
	License: \tab GLP >= 2\cr
	}

QUICK INSTRUCTIONS:
1) start R by clicking on icon and entering command
2) define .UserDirectory variable and .First function as described below.
3) enter command: save.image()
4) q()

`.UserDirectory`
This defines where you want to save your R workspaces. 
For Microsoft Windows, I use: .UserDirectory <- "d:/r"
For Mac OS X, I use: .UserDirectory <- "/Users/aim/R"
For unix, I use: .UserDirectory <- "/home/faculty/aim/R"

`.First`
My .First function:
> .First
function(){
library(rwm)
}

SUMMARY

The main functions are `loadws()`, and `savews()`.
Also `cleanws()` and `continuews()` are provided.
These functions, where were  
loosely inspired by APL commands: )LOAD, )SAVE, )CONTINUE, and )CLEARWS
are useful for managing R workspaces.
See Anscombe (1981, p.103) or Grenander (1982, p.151-152).
This package uses hidden global variables in the user's workspace:
.UserDirectory, .UserDate,
.WSID, .LastSaved and .Describe.

}

\author{A.I. McLeod}

\note{
During installation you should ignore warnings about
no visible binding for global variables
`.WSID` and `.UserDirectory` since these are
assigned by the user after the package has been installed.
These settings are system and user dependent.
}

\section{Upgrading}{
When upgrading to a new version of `rwm` you need to make that you are not
already attached to the package. Use the `detach(...)` function to detach the
library if necessary.
}

\references{
Ansombe, F.J. (1981),
Computing in Statistical Science Through APL,
Springer-Verlag.

McLeod, A.I. (2008). R Workspace Management: With R Package.
}

\keyword{ package }
\keyword{environment}

\examples{
#Simple Example
\dontrun{
.UserDirectory<-R.home()
savews("myws")
#Note: instead of R.home() you would normally use some other directory
}

}




