\name{OptionalNumeric-class}
\Rdversion{1.1}
\docType{class}
\alias{OptionalNumeric-class}

\title{Class "OptionalNumeric"  }
\description{	 
    An S4 class union, whose member classes are class \code{"NULL"} and class \code{"numeric"}.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
  \describe{
    \item{doExperiment}{
        \code{signature(designSpecs = "list", popModelSpecs = "list", outcomeModelSpecs = "list", 
        evalSpecs = "list", nReps = "numeric", seed = "OptionalNumeric", 
        simDataDir = "OptionalCharacter")}: performs a simulation experiment and 
        returns evaluation results.  }
	 }
}

\seealso{
	\link{doExperiment}
}

\examples{
showClass("OptionalNumeric")
}

\keyword{classes}
