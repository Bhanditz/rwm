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
	Version: \tab 1.24\cr
	Date: \tab 2008-08-20\cr
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

The main functions are `loadws()`, `savews()` and `cleanws()` for managing
R workspaces. 
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
Loosely inspired by APL commands )LOAD, )SAVE and )CLEARWS. See:
Ansombe, F.J. (1981),
Computing in Statistical Science Through APL,
Springer-Verlag.
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




