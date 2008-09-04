\name{InitializeRWM}
\alias{InitializeRWM}

\title{ Initialize the setup for the `rwm` package }
\description{
Provides a simple method of initializing your R workspace so you
can use the `rwm` package. This function is normally only ever used one
time.  Initialization is easily done manually without this function at all.
See below for manual instructions.
}
\usage{
InitializeRWM()
}


\details{
You can run this function or do it manually as described below:

MANUAL METHOD
1) start R by clicking on icon and entering command
2) define variables: `.UserDirectory` and `.UserDate`
3) define function .First function as described below.
3) enter command: save.image()
4) q()

`.UserDirectory`
This defines where you want to save your R workspaces. 
For Microsoft Windows, I use: .UserDirectory<-"d:/r/"
For Mac OS X, I use: .UserDirectory<-"/Users/aim/r/"
For unix, I use: .UserDirectory<-/home/aim/R/

`.UserDate`
I use: .UserDirectory<-2008
This defines the subdirectory for current R workspace directories.
To ignore this, option just set .UserDirectory<-2008 or perhaps .UserDirectory<-"current"
Another possible if might be .UserDirectory<-"2008-Jan".  

`.First`
My .First function:
> .First
function(){
library(rwm)
}

Of course there are other possible libraries or initializations you might want to add to
your `.First` function.


}
\value{
The global variable `.UserDirectory` and  `.UserDate` are defined.
}

\references{ 
McLeod, A.I. (2008). R Workspace Management: With R Package.
}

\author{ A.I. McLeod }

\note{
If an invalid `.UserDirectory` is given, an error results
and the function terminates.

This function makes extensive uses of the R File and Directory Manipulation functions.
}

\seealso{ 
\code{\link{files}},
\code{\link{save.image}}
 }

\examples{

\dontrun{
InitializeRWM() #you will be prompted for `.UserDirectory` and `.UserDate`
}

}




\keyword{environment}
