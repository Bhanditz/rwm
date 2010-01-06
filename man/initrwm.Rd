\name{initrwm}
\alias{initrwm}

\title{ Initialize the setup for the rwm package }
\description{
Provides a simple method of initializing your R workspace so you
can use the \pkg{rwm} package. This function is normally only ever used one
time.  Initialization is easily done manually without this function at all.
See below for manual instructions.
}
\usage{
initrwm()
}


\details{
You can run this function or do it manually as described below:

MANUAL METHOD

\enumerate{
\item{1)}{ start R by clicking on icon and entering command}
\item{2)}{ define variables: \code{.UserDirectory} and \code{.UserDate}}
\item{3)}{ define function \code{.First} function as described below}
\item{4)}{ enter command: \command{save.image()}}
\item{4)}{ \command{q()}}
}

\code{.UserDirectory}
This defines where you want to save your R workspaces. 
For Microsoft Windows, I use: \command{.UserDirectory<-"d:/r/"}.
For Mac OS X, I use: \command{UserDirectory<-"/Users/aim/r/"}.
For unix, I use: \command{.UserDirectory<-/home/aim/R/}.

\code{.UserDate}
Currently I use: \command{UserDirectory<-2009}.
This defines the subdirectory for current R workspace directories.
To ignore this, option just set \command{.UserDirectory<-""} or perhaps. 
\command{UserDirectory<-"current"}
Another possible if might be \command{.UserDirectory<-"2008-Jan"}.  

\code{.First}
My \code{.First} function with Windows:
\code{ .First <- function() library(RWinEdt)}

Of course there are other possible libraries or initializations you might want to add to
your \code{.First} function.


}
\value{
The global variable \code{.UserDirectory} and  \code{.UserDate} are defined.
}

\author{ A.I. McLeod }

\note{
If an invalid \code{.UserDirectory} is given, an error results
and the function terminates.

This function makes extensive uses of the R File and Directory Manipulation functions.
}

\seealso{ 
\code{\link{files}},
\code{\link{save.image}}
 }

\examples{

#you will be prompted for '.UserDirectory' and '.UserDate'

\dontrun{
initrwm() 
}

}




\keyword{environment}
