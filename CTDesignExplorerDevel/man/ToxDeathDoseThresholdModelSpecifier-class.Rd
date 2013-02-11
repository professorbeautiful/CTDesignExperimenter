\name{ToxDeathDoseThresholdModelSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{ToxDeathDoseThresholdModelSpecifier-class}

\title{Class "ToxDeathDoseThresholdModelSpecifier"  }
\description{	
   An S4 class, representing the specifications for a dose threshold model which generates
   outcomes for both toxicity and death.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ToxDeathDoseThresholdModelSpecifier", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DeltaDeath}:}{
       Object of class \code{"numeric"}. A patient will die if the assigned dose > toxicity
       dose threshold * DeltaDeath.  }
  }
}

\section{Extends}{
Class \code{"\linkS4class{OutcomeModelSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{generateOutcomes}{
        \code{signature(outcomeModelSpec = "ToxDeathDoseThresholdModelSpecifier", 
        thisPatCurrentData = "PatData")}: generates outcomes from most recent treatment for both 
        toxicity and death from a dose threshold model.  }
    \item{getProvisions}{
        \code{signature(spec = "ToxDeathDoseThresholdModelSpecifier")}: gets a list of names of
        the items that \code{spec} provides.  }
    \item{getRequirements}{
        \code{signature(spec = "ToxDeathDoseThresholdModelSpecifier")}: gets a list of names of
        the items that \code{spec} requires.  }
	 }
}

\examples{
showClass("ToxDeathDoseThresholdModelSpecifier")
}

\keyword{classes}
