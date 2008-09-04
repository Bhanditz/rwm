\name{TestRWMSetup}
\alias{TestRWMSetup}

\title{ Test setup for rwm package }

\description{Tests that the variable `.UserDirectory` has a valid setting.}

\usage{TestRWMSetup()}

\details{
Four tests are done. First that `.UserDirectory` is set and second that it is set
to a valid directory. These two tests are repeated for `.UserDate`.
Uses built-in R functions `exists()` and `file.exists()`.
}

\value{
Returns TRUE or FALSE according if all 4 tests are passed or not.
}

\references{ 
McLeod, A.I. (2008). R Workspace Management: With R Package.
}

\author{A.I. McLeod }

\note{ This function is used by most of the other functions to validate
the setup before attempting to run.
The global variable `.UserDirectory` may be set manually as in the examples
below and it can be set by running 'InitializeRWM()`. 
}

\seealso{
\code{\link{loadws}},
\code{\link{savews}},
\code{\link{InitializeRWM}}
}

\examples{
#These examples work on my Windows computer but they are easily modified to other
# computers running MacOS or Linux
#Windows Example
\dontrun{
.UserDirectory<-NULL
#generates error
TestRWMSetup()
#now set to a valid directory
.UserDirectory<-"d:/r"
TestRWMSetup()
}

#Mac OS Example
\dontrun{
.UserDirectory<-NULL
#generates error
TestRWMSetup()
#now set to a valid directory
.UserDirectory<-"/Users/aim/R"
TestRWMSetup()
}

#Linnux Example
\dontrun{
.UserDirectory<-NULL
#generates error
TestRWMSetup()
#now set to a valid directory
.UserDirectory<-"/home/faculty/aim/R"
TestRWMSetup()
}

}
\keyword{environment}
