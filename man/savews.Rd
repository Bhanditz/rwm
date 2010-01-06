\name{savews}
\alias{savews}

\title{ Save workspace }
\description{
Save R workspace and history.
}
\usage{savews(name = .WSID, d = as.character(.UserDate), silentQ = FALSE, 
      historyQ = TRUE, q = FALSE, prefix = .Prefix)
}

\arguments{
  \item{name}{name of workspace  }
  \item{d}{ usually year but could be any other part  }
  \item{silentQ}{ TRUE, no message}
  \item{historyQ}{ TRUE, save R history }
  \item{q}{ quit R, FALSE - no; TRUE - yes quit  }
  \item{prefix}{Default workspace filename is \sQuote{.Rdata} but if \code{prefix} is a valid
  	non-null character string, it will be prepended to \dQuote{.Rdata}. 
	If the argument \code{prefix} is not supplied and if \code{.Prefix} does not exist, 
	\code{prefix} is set to \code{NULL}.  }
}

\value{
Just before saving, the following is done:

\item{1}{the global variable \code{.WSID} is set to the working directory for the saved workspace}
\item{2}{the global variable \code{.UserDate} is set}
\item{3}{if prefix is non-Null, the global variable \code{.Prefix} is set}
 
}

\details{
First the working directory is changed to that specified by the arguments.
The working directory is specified by concatenating 
\sQuote{.UserDirectory} \code{<>} \dQuote{d} \code{<>} \sQuote{name} unless
\sQuote{name} is set to its default \code{.WSID}. 
In this case the working directory is simply \code{.WSID}. 
Then \code{save.image(.Rdata)}  and \code{savehistory(.Rhistory)} are used.
The global variable \code{.WSID} is set to the working directory.
If \code{prefix} is supplied, it is prepended to \dQuote{.Rdata} and to \dQuote{.Rhistory}.

An error message is given if \code{.UserDirectory} is not defined.
The \code{.UserDirectory} may be set manually using a normal
R assignment or by the function \code{IntializeRWM}.

A warning is given if prefix is invalid. In this case
the workspace is saved as \dQuote{.Rdata}.
}

\author{A.I. McLeod }

\note{ \code{savews()} is a useful shortcut. It saves the current workspace
in the same file it was in when loaded. }

\seealso{ 
\code{\link{load}},
\code{\link{savews}},
\code{\link{clearws}},
\code{\link{initrwm}}
}

\examples{

#INITIALIZATION
#Normally .UserDirectory and .UserDate are defined previously. 
#Usually a more convenient directory is used but for illustration
#using a script which will run in interactive or batch mode on any computer:
.UserDirectory <- tempdir()
.UserDate <- "2008"
dir.create(paste(.UserDirectory, .UserDate, sep="/"))

#Example 1. Define function to compute all primes <=n
#Save this and also a list of all primes less than 200
primes <-  function(n){
 (2:n)[apply(outer(2:n, 2:n, FUN="\%\%")==0, MARGIN=1, sum)==1]
 }
ListPrimes<-primes(200)
savews("Primes")
#Now that .WSID has been defined, you can save this workspace with
savews()
#Notice that an error is produced if the argument 'name' is not a 
#  character string
\dontrun{
savews(Primes)
}

#Example 2. Compute a larger list and save new results
ListPrimes<-primes(1000)
savews()
# Note that no argument is required for 'savews()'


}
\keyword{environment}



