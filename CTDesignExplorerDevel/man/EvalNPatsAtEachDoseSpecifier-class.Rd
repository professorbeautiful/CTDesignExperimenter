\name{EvalNPatsAtEachDoseSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalNPatsAtEachDoseSpecifier-class}

\title{Class "EvalNPatsAtEachDoseSpecifier"  }
\description{	 
    An S4 class, representing the specification for evaluating the number of patients allocated 
    at each dose.  }
    
\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("EvalNPatsAtEachDoseSpecifier", ...)}.
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
        \code{signature(evalSpec = "EvalNPatsAtEachDoseSpecifier", simCTsData = "list")}: evaluates 
        the number of patients allocated at each dose. }
    \item{getRequirements}{\code{signature(spec = "EvalNPatsAtEachDoseSpecifier")}: gets a 
        list of names of the items \code{spec} requires.  }
	 }
}

\examples{
showClass("EvalNPatsAtEachDoseSpecifier")
}
\keyword{classes}
