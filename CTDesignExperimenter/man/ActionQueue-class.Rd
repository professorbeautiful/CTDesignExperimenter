\name{ActionQueue-class}
\Rdversion{1.1}
\docType{class}
\alias{ActionQueue-class}

\title{Class "ActionQueue"  }
\description{	 An S4 class represents action queues.  }
\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("ActionQueue", ...)}.
	An action queue contains one slot for a list of objects of class \code{"\linkS4class{Action}"} 
    ordered by the times when they are executed. 
}

\section{Slots}{
  \describe{
    \item{\code{ActionQ}:}{
        Object of class \code{"list"}, a list of objects of class \code{"\linkS4class{Action}"}.  }
  }
}

\section{Methods}{
  \describe{
    \item{addAction}{
        \code{signature(currentActionQ = "ActionQueue", newAction = "Action")}: adds a new object of 
        class \code{"\linkS4class{Action}"} to an action queue.  }
	 }
}

\seealso{
	 \code{\link{addAction}},
     \code{\link{getOtherArgs}},
	 \code{\linkS4class{Action}} 
}

\examples{
showClass("ActionQueue")
}

\keyword{classes}
