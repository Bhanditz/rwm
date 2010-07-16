\name{testrwm}
\alias{testrwm}

\title{ Test setup for rwm package }

\description{Tests that the variable \code{.UserDirectory} has a valid setting.}

\usage{testrwm()}

\details{
Returns TRUE if all of the following are true.
First that \code{.UserDirectory} is set and second that it is set
to a valid directory. 
Second \code{.UserDate}  must be set to valid directory name.
Third either the directory specificed by \code{.UserDirectory <> .UserDate} exists
or the base function \code{dir.create} is successful in creating it.

Uses base R functions \code{exists}, \code{file.exists} and \code{dir.create}.
}

\value{
Returns TRUE or FALSE according if the setup is valid
}

\author{A.I. McLeod }

\note{ This function is used by most of the other functions to validate
the setup before attempting to run.
The global variable \code{.UserDirectory} may be set manually as in the examples
rwm/
rwm/g
rwm/g
rwm/g
rwm/g
rwm/g
rwm/g
below and it can be set by running \command{rwmInit()}. 
}

\seealso{
\code{\link{loadws}},
\code{\link{savews}},
rwm/
rwm/g
rwm/g
rwm/g
rwm/g
rwm/g
rwm/g
\code{\link{rwmInit}}
}

\examples{
#INITIALIZATION
#Normally .UserDirectory and .UserDate are defined previously. 
#Usually a more convenient directory is used but for illustration
#using a script which will run in interactive or batch mode on any computer:
.UserDirectory <- tempdir()
.UserDate <- "2008"
dir.create(paste(.UserDirectory, .UserDate, sep="/"))

# Example 1: test the above setup is valid
testrwm()

#Example 2: .UserDirectory is not set
rm(.UserDirectory)
testrwm()
}

\keyword{environment}
