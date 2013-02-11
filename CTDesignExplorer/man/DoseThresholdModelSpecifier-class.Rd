\name{DoseThresholdModelSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{DoseThresholdModelSpecifier-class}

\title{Class "DoseThresholdModelSpecifier"  }
\description{
    An S4 class, representing specifications of the outcome models based on the dose thresholds.
}

\section{Objects from the Class}{
   Objects can be created by calls of the form \code{new("DoseThresholdModelSpecifier", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{DoseThresholdName}:}{Object of class \code{"character"}, the name of 
    dose threshold, which can either "ToxDoseThreshold" (dose threshold for toxicity response) or
    "EfficacyDoseThreshold" (dose threshold for efficacy response).  }
  }
}

\section{Extends}{
Class \code{"\linkS4class{OutcomeModelSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{generateOutcomes}{
        \code{signature(outcomeModelSpec = "DoseThresholdModelSpecifier",thisPatCurrentData = 
        "PatData")}: generates an outcome from a patient's most recent treatment.  }
    \item{getProvisions}{
        \code{signature(spec = "DoseThresholdModelSpecifier")}: gets a list of names of the items
        that \code{spec} provides.  }
    \item{getRequirements}{
        \code{signature(spec = "DoseThresholdModelSpecifier")}: gets a list of names of the items
        that \code{spec} requires.  }
	 }
}

\note{ 
    If the assigned dose > dose threshold, a patient will have toxicity/efficacy response, otherwise
    a patient will not have response.
}

\examples{
showClass("DoseThresholdModelSpecifier")
}
\keyword{classes}
