\name{EvalProbSuccPhase2Specifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalProbSuccPhase2Specifier-class}

\title{Class "EvalProbSuccPhase2Specifier"  }
\description{	 
   An S4 class, representing the specification for evaluating the probability of 
   success in a hypothetical Phase II trial which uses Phase II Bryant and Day  design.  
}
   
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EvalProbSuccPhase2Specifier", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{LogToxDoseThresholdMean}:}{
        Object of class \code{"numeric"}, the mean log(toxicity dose threshold).  }
    \item{\code{LogToxDoseThresholdSd}:}{
        Object of class \code{"numeric"}, the standard deviation of log(toxicity dose
        threshold) distribution.  }
    \item{\code{LogEfficacyThresholdMean}:}{
        Object of class \code{"numeric"}, the mean log(efficacy dose threshold).  }
    \item{\code{LogEfficacyDoseThresholdSd}:}{
        Object of class \code{"numeric"}, the standard deviation of log(efficacy dose
        threshold) distribution.  }
    \item{\code{Phase2DesignSpec}:}{
        Object of class \code{"\linkS4class{Phase2BryantDaySpecifier}"}, the specification for a Phase II
        Bryant and Day Design.  }
  }
}

\section{Extends}{
Class \code{"\linkS4class{EvalSpecifier}"}, directly.
}

\section{Methods}{
No methods defined with class "EvalProbSuccPhase2Specifier" in the signature.
}

\references{ 
    Bryant J. and Day R. 1995 Incorporating Toxicity Considerations Into the Design 
    of Two-Stage Phase II Clinical Trials, \emph{Biometrics}, \bold{51}, 1372--1383. 
}

\note{
	Efficacy and toxicity dose thresholds for the patients in the Phase II trials are assumed
    to follow bi-lognormal distribution.  We will implement the associated method for evaluating
    the probability of success in a hypothetical Phase II trial which uses Phase II Bryant and Day  
    design soon. 
}

\examples{
showClass("EvalProbSuccPhase2Specifier")
}

\keyword{classes}
