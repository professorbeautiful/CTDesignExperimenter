\name{EvalRP2DSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalRP2DSpecifier-class}

\title{Class "EvalRP2DSpecifier"  }
\description{	 
    An S4 class, representing the specification for evaluating the recommend Phase II dose (RP2D).
}

\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("EvalRP2DSpecifier", ...)}.
}

\section{Extends}{
Class \code{"\linkS4class{EvalSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{evalDesign}{
        \code{signature(evalSpec = "EvalRP2DSpecifier", simCTsData = "list")}: evaluates 
        the chosen RP2Ds. It returns a list of two elements: Summary (mean, variance
        number of \code{NA}s) and a vector of chosen RP2Ds.  }
    \item{getRequirements}{
        \code{signature(spec = "EvalRP2DSpecifier")}: gets a list of names of the items that
        \code{spec} requires.  }
	 }
}

\examples{
showClass("EvalRP2DSpecifier")
}

\keyword{classes}
