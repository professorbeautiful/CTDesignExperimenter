\name{getProvisions-methods}
\docType{methods}
\alias{getProvisions-methods}
\alias{getProvisions,APlusBSpecifier-method}
\alias{getProvisions,BaseCharModelSpecifier-method}
\alias{getProvisions,CRMSpecifier-method}
\alias{getProvisions,DoseThresholdModelSpecifier-method}
\alias{getProvisions,NoParamProbModelSpecifier-method}
\alias{getProvisions,OptionalPopModelSpecifier-method}
\alias{getProvisions,ToxDeathDoseThresholdModelSpecifier-method}
\alias{getProvisions,ToxEfficacyDoseThresholdsModelSpecifier-method}
\alias{getProvisions}

\title{ Methods "getProvisions"  }
\description{
    gets a list of names of the items that an object provides.
}

\section{Methods}{
\describe{

    \item{\code{signature(spec = "APlusBSpecifier")}}{
        gets a list of names of the items that a "A+B" with dose de-escalation design provides. }

    \item{\code{signature(spec = "BaseCharModelSpecifier")}}{
         gets the name of the baseline characteristic that a baseline characteristic model provides.  }

    \item{\code{signature(spec = "CRMSpecifier")}}{
         gets a list of names of the items that a CRM design provides.  }

    \item{\code{signature(spec = "DoseThresholdModelSpecifier")}}{ 
         gets a list of names of the items that a dose threshold outcome model provides.  }

    \item{\code{signature(spec = "NoParamProbModelSpecifier")}}{
         gets a list of names of the items that an unmodelled probability model provides.  }

    \item{\code{signature(spec = "OptionalPopModelSpecifier")}}{ 
         gets a list of names of the items that an object of \code{"linkS4class{OptionalPopModelSpecifier}"}
         provides.  }

    \item{\code{signature(spec = "ToxDeathDoseThresholdModelSpecifier")}}{ 
         gets a list of names of the items that a dose threshold model for both toxicity and death
         outcomes provides.  }

    \item{\code{signature(spec = "ToxEfficacyDoseThresholdsModelSpecifier")}}{ 
         gets a list of names of the items that a dose threshold model for both toxicity and 
         efficacy outcomes provides.  }
}}

\note{
    The returned value is a named list of different lengths according to the signature. 
    When \code{spec} is specification for a design, the returned value is a named list of length 3 with 
    element names: "TrtAllos", "CTTimes" and "Conclusions".
    When \code{spec} is specification for an outcome Model, the returned value is a named list of 
    length 2 with element names: "Outcomes", "TimestoOutcomes".
    When \code{spec} is specification for an object of \code{"OptionalPopModelSpecifier"}, the 
    returned value is a named list of length 1 with the element name as "BaseChars".
    Each element in the returned list is a character vector of length >= 1 with at least one string
    for "None",which is technically necessary when using the R function \code{\link{match}} for
    implementing name matching mechanisms.
}

\examples{
    # see what "3+3" with dose de-escalation design provides.
    ThreePlusThreeSpec <- new("APlusBSpecifier",TierDoses=c(3.0, 6.0, 9.9, 15.0, 21.1, 28.0))
    getProvisions(spec=ThreePlusThreeSpec)
}

\keyword{methods}
