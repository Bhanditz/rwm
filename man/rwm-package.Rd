\name{rwm-package}
\alias{rwm-package}
\alias{rwm}
\docType{package}
\title{R Workspace Management}

\description{Functions for managing R workspaces}

\details{
	\tabular{ll}{
	Package: \tab rwm\cr
	Type: \tab Package\cr
	Version: \tab 1.51\cr
	Date: \tab 2010-07-27\cr
	License: \tab GLP >= 2\cr
	}

QUICK INSTRUCTIONS:

\enumerate{
\item{\strong{Start: }}{Start R with blank workspace.}
\item{\strong{Definitions: }}{Define the variables \code{.UserDirectory}
and \code{.UserDate}.}
\item{\strong{Save}}{Save the workspace with the command \command{save.image()}.}
\item{\strong{Quit}}{Quit R, \command{q()}}.
}


\code{.UserDirectory}

This defines where you want to save your R workspaces.
Here are some examples for different operating systems.
\enumerate{
\item{\strong{Windows:}}{\code{.UserDirectory <- "d:/r"} }
\item{\strong{Mac OS X}}{\code{.UserDirectory <- "/Volumes/MacAIM/R"}}
\item{\strong{unix}}{\code{.UserDirectory <- "/home/faculty/aim/R"} }
}


SUMMARY

The main functions are \code{loadws}, \code{savews} and \code{attachws}.
Additionally, \code{cleanws}  and \code{cws} are provided.
These functions, where were  
loosely inspired by APL commands: \command{)LOAD}, \command{)SAVE}, \command{)LIB}, \command{)CONTINUE}, and 
and \command{)CLEARWS}.
This package uses hidden global variables in the user's workspace:
\code{.UserDirectory}, \code{.UserDate},
\code{.WSID}, \code{.Prefix}, \code{.LastSaved} and \code{.Describe}.
}

\author{A.I. McLeod}

\note{
During installation you should ignore warnings about
no visible binding for global variables
\code{.WSID} and \code{.UserDirectory} since these are
assigned by the user after the package has been installed.
These settings are system and user dependent.
}

\references{
McLeod, A.I. (2009). \pkg{rwm}: An R Package for Workspace Management
}

\keyword{ package }
\keyword{environment}

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
#The workspace MyWs contains the variables p, n, e, X

}





