\name{EvalProbRP2DAtEachDoseSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalProbRP2DAtEachDoseSpecifier-class}

\title{Class "EvalProbRP2DAtEachDoseSpecifier"  }
\description{
    An S4 class, representing the specification for evaluating the 
    probability of each dose being chosen as the recommended Phase II dose (RP2D).
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EvalProbRP2DAtEachDoseSpecifier", ...)}.
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
        \code{signature(evalSpec = "EvalProbRP2DAtEachDoseSpecifier", simCTsData = "list")}: 
        evaluates the probability of each dose being chosen as the recommended Phase II dose
        (RP2D). }
    \item{getRequirements}{
        \code{signature(spec = "EvalProbRP2DAtEachDoseSpecifier")}: gets a list of names of the
        items that \code{spec} requires.  }
     }
}

\examples{
showClass("EvalProbRP2DAtEachDoseSpecifier")
}
\keyword{classes}
