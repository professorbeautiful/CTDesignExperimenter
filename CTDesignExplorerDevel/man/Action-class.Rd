\name{Action-class}
\Rdversion{1.1}
\docType{class}
\alias{Action-class}

\title{Class "Action"}
\description{	
    An S4 class, representing elements listed in an action queue
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Action", ...)}. 
}
\section{Slots}{
  \describe{
    \item{\code{MethodCall}:}{
        Object of class \code{"character"}, containing a string of the S4 method call which is 
        evaluated when the action is executed.  }
    \item{\code{OtherArgs}:}{
        Object of class \code{"list"}, a named list of the arguments in the \code{MethodCall} whose 
        values are obtained in the environment from which the action is created.  }
    \item{\code{GlobalTime}:}{
        Object of class \code{"numeric"}, time or order that the \code{MethodCall} is evaluated.  }
  }
}

\section{Methods}{
  \describe{
    \item{addAction}{
       \code{signature(currentActionQ = "ActionQueue", newAction = "Action")}: adds a new object of 
        class \code{"Action"} to an action queue.  }
    \item{getOtherArgs}{
        \code{signature(action = "Action")}: assigns the "other" arguments' values to 
        their names in the parent frame where the action is executed.  }
	 }
}

\note{
    Here, executing an action is the same as calling an S4 method which is specified
    in the slot \code{MethodCall} of that action.
}

\seealso{
     \code{\link{getOtherArgs}},
     \code{\link{addAction}},
     \code{\linkS4class{ActionQueue-class}}
}

\examples{
showClass("Action")
}

\keyword{classes}
