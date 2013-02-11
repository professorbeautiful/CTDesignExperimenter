\name{EvalNToxsAtEachDoseSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalNToxsAtEachDoseSpecifier-class}

\title{Class "EvalNToxsAtEachDoseSpecifier"  }
\description{	
An S4 class, representing the specification for evaluating the number of binary toxicity outcomes 
observed at each dose.  }

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EvalNToxsAtEachDoseSpecifier", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{TierDoses}:}{Object of class \code{"numeric"}, a vector of doses for testing.  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{EvalSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{evalDesign}{
        \code{signature(evalSpec = "EvalNToxsAtEachDoseSpecifier", simCTsData = "list")}: evaluates 
        the number of binary toxicity outcomes observed at each dose.  }
    \item{getRequirements}{
        \code{signature(spec = "EvalNToxsAtEachDoseSpecifier")}: gets a list of names of items
        that \code{spec} requires.  }
	 }
}

\examples{
showClass("EvalNToxsAtEachDoseSpecifier")
}
\keyword{classes}
