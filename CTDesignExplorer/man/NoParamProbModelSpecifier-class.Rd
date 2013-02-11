\name{NoParamProbModelSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{NoParamProbModelSpecifier-class}

\title{Class "NoParamProbModelSpecifier"  }
\description{	
    An S4 class, representing the specification for an unmodelled probability model.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NoParamProbModelSpecifier", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{Probs}:}{
         Object of class \code{"matrix"}, with ncol=# Tier Doses, nrow=# subpopulations, 
         and row index = subpopulation index. }
    \item{\code{TierDoses}:}{
         Object of class \code{"numeric"}, a vector of dose for testing.  }
    \item{\code{OutcomeName}:}{
         Object of class \code{"character"}, which is either the name of a binary toxicity outcome
         or efficacy outcome.  }
  }
}

\section{Extends}{
Class \code{"\linkS4class{OutcomeModelSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{generateOutcomes}{
        \code{signature(outcomeModelSpec = "NoParamProbModelSpecifier", 
        thisPatCurrentData = "PatData")}: generates an outcome from a patient's most recent treatment based on an unmodelled
        probability outcome model.  }
    \item{getProvisions}{
        \code{signature(spec = "NoParamProbModelSpecifier")}: gets a list of names of the items
        an unmodelled probability model provides.  }
    \item{getRequirements}{
         \code{signature(spec = "NoParamProbModelSpecifier")}: gets a list of names of the items
         an unmodelled probability model provides.  }
	 }
}

\examples{
showClass("NoParamProbModelSpecifier")
}

\keyword{classes}
