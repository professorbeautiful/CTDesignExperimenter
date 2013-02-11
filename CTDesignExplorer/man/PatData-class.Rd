\name{PatData-class}
\Rdversion{1.1}
\docType{class}
\alias{PatData-class}

\title{Class "PatData"  }
\description{	 
  An S4 class, representing a patient's data objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PatData", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{ID}:}{
        Object of class \code{"OptionalNumeric"}, a patient's identification number in a 
        clinical trial.  }
    \item{\code{BaseChars}:}{
        Object of class \code{"OptionalNumeric"}, a patient's baseline characteristics.  }
    \item{\code{ConcurrentTrtsDataList}:}{Object of class \code{"list"}, a list of data from
        each concurrent treatments.  }
    \item{\code{PatTimes}:}{Object of class \code{"OptionalNumeric"}, a vector of times when
        events at patient level occurs, such as the time when a patient gets off study.  }
  }
}

\section{Methods}{
  \describe{
    \item{generateOutcomes}{
        \code{signature(outcomeModelSpec = "DoseThresholdModelSpecifier", 
        thisPatCurrentData = "PatData")}: generates an outcome from the most recent treatment 
        based on a dose threshold model.  }
    \item{generateOutcomes}{
        \code{signature(outcomeModelSpec = "ToxDeathDoseThresholdModelSpecifier", 
        thisPatCurrentData = "PatData")}: generates outcomes for both toxicity and death from 
        the most recent treatment based on a dose threshold model.  }
    \item{generateOutcomes}{
        \code{signature(outcomeModelSpec = "ToxEfficacyDoseThresholdsModelSpecifier", 
        thisPatCurrentData = "PatData")}: generates outcomes for both toxicity and efficacy from 
        the most recent treatment based on a dose threshold model.  }
    \item{generateOutcomes}{
        \code{signature(outcomeModelSpec = "NoParamProbModelSpecifier", 
        thisPatCurrentData = "PatData")}: generates an outcome from  the most recent treatment 
        based on an unmodelled probability model.  }
	 }
}

\seealso{
	\linkS4class{ConcurrentTrtsData},
    \linkS4class{CTData}
}

\examples{
showClass("PatData")
}

\keyword{classes}
