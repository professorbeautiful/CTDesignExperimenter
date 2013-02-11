\name{EvalPctPatsAtEachDoseSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalPctPatsAtEachDoseSpecifier-class}

\title{Class "EvalPctPatsAtEachDoseSpecifier"  }
\description{	
    An S4 class, representing the specification for evaluating the 
    percentage of patients allocated at each dose.  }
    
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EvalPctPatsAtEachDoseSpecifier", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{TierDoses}:}{
      Object of class \code{"numeric"}, a vector of doses for testing.  }
  }
}

\section{Extends}{
Class \code{"\linkS4class{EvalSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{evalDesign}{
        \code{signature(evalSpec = "EvalPctPatsAtEachDoseSpecifier", simCTsData = "list")}: evaluates
        the percentage of patients allocated at each dose.  }
    \item{getRequirements}{
        \code{signature(spec = "EvalPctPatsAtEachDoseSpecifier")}: gets a list of names of items
        that \code{spec} requires.  }
	 }
}


\examples{
showClass("EvalPctPatsAtEachDoseSpecifier")
}

\keyword{classes}
