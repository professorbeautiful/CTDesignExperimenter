\name{EvalToxRateSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalToxRateSpecifier-class}

\title{Class "EvalToxRateSpecifier"  }
\description{	 
    An S4 class, representing the specification for evaluating the average toxicity rate.  }

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EvalToxRateSpecifier", ...)}.
}

\section{Extends}{
Class \code{"\linkS4class{EvalSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{evalDesign}{
        \code{signature(evalSpec = "EvalToxRateSpecifier", simCTsData = "list")}: evaluates
        the average toxicity rate.  }
    \item{getRequirements}{
        \code{signature(spec = "EvalToxRateSpecifier")}: gets a list of names of the items that
        \code{spec} requires.  }
	 }
}

\examples{
showClass("EvalToxRateSpecifier")
}
\keyword{classes}
