\name{EvalNToxsSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalNToxsSpecifier-class}

\title{Class "EvalNToxsSpecifier"  }
\description{	 
    An S4 class, representing the specification for evaluating the total number of 
    observed binary toxicity outcomes.  }
    
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EvalNToxsSpecifier", ...)}.
}

\section{Extends}{
Class \code{"\linkS4class{EvalSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{evalDesign}{
        \code{signature(evalSpec = "EvalNToxsSpecifier", simCTsData = "list")}: evaluates 
        the total number of observed binary toxicity outcomes. It returns a list of two elements: 
        Summary (mean and variance) and a vector of total number of observed binary toxicity 
        outcomes.  }
    \item{getRequirements}{\code{signature(spec = "EvalNToxsSpecifier")}: gets a list of names of
        the iterms that \code{spec} requires.  }
	 }
}

\examples{
showClass("EvalNToxsSpecifier")
}
\keyword{classes}
