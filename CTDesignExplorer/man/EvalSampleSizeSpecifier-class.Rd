\name{EvalSampleSizeSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalSampleSizeSpecifier-class}

\title{Class "EvalSampleSizeSpecifier"  }
\description{	 
   An S4 class, representing the specification for evaluating the sample size.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EvalSampleSizeSpecifier", ...)}.
}

\section{Extends}{
Class \code{"\linkS4class{EvalSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{evalDesign}{
        \code{signature(evalSpec = "EvalSampleSizeSpecifier", simCTsData = "list")}: }
    \item{getRequirements}{\code{signature(spec = "EvalSampleSizeSpecifier")}: evaluates 
    the sample sizes.It returns a list of two elements: Summary (mean and variance) and a 
    vector of sample sizes.  }
	 }
}

\examples{
showClass("EvalSampleSizeSpecifier")
}
\keyword{classes}
