\name{ToxEfficacyDoseThresholdsModelSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{ToxEfficacyDoseThresholdsModelSpecifier-class}

\title{Class "ToxEfficacyDoseThresholdsModelSpecifier" }
\description{	
    An S4 class, representing the specifications for a dose threshold model which generates
    outcomes for both toxicity and efficacy responses.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ToxEfficacyDoseThresholdsModelSpecifier", ...)}.
}
\section{Extends}{
Class \code{"\linkS4class{OutcomeModelSpecifier}"}, directly.
}
\section{Methods}{
  \describe{
    \item{generateOutcomes}{
        \code{signature(outcomeModelSpec = "ToxEfficacyDoseThresholdsModelSpecifier", 
        thisPatCurrentData = "PatData")}: generates outcomes from the most recent treatment for
        both toxicity and efficacy responses based on a dose threshold model.  }
    \item{getProvisions}{
        \code{signature(spec = "ToxEfficacyDoseThresholdsModelSpecifier")}: gets a list of names
        of the items that \code{spec} provides.  }
    \item{getRequirements}{
        \code{signature(spec = "ToxEfficacyDoseThresholdsModelSpecifier")}: gets a list of names
        of the items that \code{spec} requires.  }
	 }
}

\examples{
showClass("ToxEfficacyDoseThresholdsModelSpecifier")
}

\keyword{classes}
