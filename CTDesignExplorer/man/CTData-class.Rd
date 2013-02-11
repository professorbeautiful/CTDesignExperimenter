\name{CTData-class}
\Rdversion{1.1}
\docType{class}
\alias{CTData-class}

\title{Class "CTData"  }

\description{
    An S4 class, representing data objects from a single replication of a clinical trial  
}

\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("CTData", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{PatsData}:}{
        Object of class \code{"list"}, a list of objects of class \code{"\linkS4class{PatData}"}.  }
    \item{\code{CTTimes}:}{
        Object of class \code{"\linkS4class{OptionalNumeric}"}, either a named numeric vector of
        times of events occurred at CT level, for example, time when the CT ends, or NULL if 
        no clinical trial times are collected.  }
    \item{\code{Conclusions}:}{
        Object of class \code{"\linkS4class{NumericLogical}"}, conclusions from a clinical trial such
        as the recommended Phase 2 dose chosen in a clinical trial.  }
  }
}

\note{ Patients' data listed in the slot \code{PatsData} are in their enrollment order.  }

\seealso{
    \code{\link{allocateTrts}},
    \code{\link{checkStoppingRule}},
    \code{\link{checkSwitchingStageRule}},
    \code{\link{generatePatsBaseChars}},
    \code{\link{generatePatsOutcomes}}
}

\examples{
showClass("CTData")
}

\keyword{classes}
