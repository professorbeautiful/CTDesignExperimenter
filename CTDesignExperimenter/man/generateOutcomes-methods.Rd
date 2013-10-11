\name{generateOutcomes-methods}
\docType{methods}
\alias{generateOutcomes-methods}
\alias{generateOutcomes,DoseThresholdModelSpecifier,PatData-method}
\alias{generateOutcomes,NoParamProbModelSpecifier,PatData-method}
\alias{generateOutcomes,ToxDeathDoseThresholdModelSpecifier,PatData-method}
\alias{generateOutcomes,ToxEfficacyDoseThresholdsModelSpecifier,PatData-method}
\alias{generateOutcomes}

\title{ Methods for "generateOutcomes"  }
\description{
    generates outcome(s) from a patient's treatments.
}

\section{Methods}{
\describe{
    \item{\code{signature(outcomeModelSpec = "DoseThresholdModelSpecifier", 
    thisPatCurrentData = "PatData")}}{
        generates an outcome from a patient's most recent treatment based on dose 
        threshold outcome model.  }

    \item{\code{signature(outcomeModelSpec = "NoParamProbModelSpecifier", 
    thisPatCurrentData = "PatData")}}{ 
        generates an outcome from a patient's most recent treatment based on an unmodelled
        probability outcome model.  }

    \item{\code{signature(outcomeModelSpec = "ToxDeathDoseThresholdModelSpecifier", 
    thisPatCurrentData = "PatData")}}{ 
        generates binary toxicity outcome and death outcome from a patient's most recent 
        treatment based on the dose threshold model.  }
   
    \item{\code{signature(outcomeModelSpec = "ToxEfficacyDoseThresholdsModelSpecifier", 
    thisPatCurrentData = "PatData")}}{ 
         generates bivariate outcomes for toxicity and efficacy from a patient's most recent
         treatment based on the dose threshold model.  }
}}

\seealso{
    \code{linkS4class{DoseThresholdModelSpecifier}},
    \code{linkS4class{NoParamProbModelModelSpecifier}},
    \code{linkS4class{ToxDeathDoseThresholdSpecifier}},
    \code{linkS4class{ToxEfficacyDoseThresholdsModelSpecifier}}
}

\keyword{methods}
