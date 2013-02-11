\name{getRequirements-methods}
\docType{methods}
\alias{getRequirements-methods}
\alias{getRequirements,APlusBSpecifier-method}
\alias{getRequirements,BaseCharModelSpecifier-method}
\alias{getRequirements,CRMSpecifier-method}
\alias{getRequirements,DoseThresholdModelSpecifier-method}
\alias{getRequirements,EvalNPatsAtEachDoseSpecifier-method}
\alias{getRequirements,EvalNToxsAtEachDoseSpecifier-method}
\alias{getRequirements,EvalNToxsSpecifier-method}
\alias{getRequirements,EvalPctPatsAtEachDoseSpecifier-method}
\alias{getRequirements,EvalProbRP2DAtEachDoseSpecifier-method}
\alias{getRequirements,EvalRP2DSpecifier-method}
\alias{getRequirements,EvalSampleSizeSpecifier-method}
\alias{getRequirements,EvalToxRateSpecifier-method}
\alias{getRequirements,NoParamProbModelSpecifier-method}
\alias{getRequirements,ToxDeathDoseThresholdModelSpecifier-method}
\alias{getRequirements,ToxEfficacyDoseThresholdsModelSpecifier-method}
\alias{getRequirements}

\title{Methods "getRequirements"  }
\description{
    gets a list of names of the items that an object requires. 
}

\section{Methods}{
\describe{

    \item{\code{signature(spec = "APlusBSpecifier")}}{
        gets a list of names of the items that a "A+B" with dose de-escalation design requires.  }

    \item{\code{signature(spec = "BaseCharModelSpecifier")}}{ 
        gets a list of the names of the conditioning baseline characteristic 
        that a baseline characteristic model requires. }

    \item{\code{signature(spec = "CRMSpecifier")}}{
        gets a list of names of the items that a CRM design requires. }

    \item{\code{signature(spec = "DoseThresholdModelSpecifier")}}{ 
        gets a list of names of the items that a dose threshold outcome model requires.  }

    \item{\code{signature(spec = "EvalNPatsAtEachDoseSpecifier")}}{
        gets a list of names of the items that are required for evaluating patient
        distribution at each dose.  }

    \item{\code{signature(spec = "EvalNToxsAtEachDoseSpecifier")}}{ 
         gets a list of names of the items that are required for evaluating the number
         of toxicity outcomes observed at each dose. }

    \item{\code{signature(spec = "EvalNToxsSpecifier")}}{ 
         gets a list of names of the items that are required for evaluating the total 
         number of observed toxicities.  }

    \item{\code{signature(spec = "EvalPctPatsAtEachDoseSpecifier")}}{
         gets a list of names of the items that are required for evaluating the percentage
         of patients allocated at each dose.  }

    \item{\code{signature(spec = "EvalProbRP2DAtEachDoseSpecifier")}}{ 
         gets a list of names of the items that are required for evaluating the probability of
         each dose being chosen as the recommended Phase II dose (RP2D).  }

    \item{\code{signature(spec = "EvalRP2DSpecifier")}}{ 
         gets a list of names of the items that are required for evaluating the chosen RP2Ds.  }

    \item{\code{signature(spec = "EvalSampleSizeSpecifier")}}{ 
         gets a list of names of the items that are required for evaluating sample sizes.  }

    \item{\code{signature(spec = "EvalToxRateSpecifier")}}{ 
         gets a list of names of the items that are required for evaluating the average toxicity
         rate.  }

    \item{\code{signature(spec = "NoParamProbModelSpecifier")}}{ 
         gets a list of names of the items that an unmodelled probability model provides. }

    \item{\code{signature(spec = "ToxDeathDoseThresholdModelSpecifier")}}{ 
         gets a list of names of the items that a dose threshold model for both toxicity and death
         outcomes requires.  }

    \item{\code{signature(spec = "ToxEfficacyDoseThresholdsModelSpecifier")}}{ 
         gets a list of names of the items that a dose threshold model for both toxicity and 
         efficacy outcomes requires.  }
}}

\note{
    The returned value is a named list of different lengths according to the signature. 
    When \code{spec} is specification for a design, the returned value is a named list of length 3 with 
    element names: "Outcomes", "TimestoOutcomes" and "BaseChars".
    When \code{spec} is specification for an outcome Model, the returned value is a named list of 
    length 2 with element names: "TrtAllos", "BaseChars".
    When \code{spec} is specification for an evaluation criterion, the 
    returned value is a named list of length 5 with the element names: "BaseChars", "Outcomes",
    "CTTimes", "TrtAllos" and "Conclusions".
    Each element in the returned list is a character vector of length >= 1 with at least one string
    for "None",which is technically necessary when using the R function \code{\link{match}} for
    implementing name matching mechanisms.
}

\examples{
    # see what "3+3" with dose de-escalation design requires.
    ThreePlusThreeSpec <- new("APlusBSpecifier",TierDoses=c(3.0, 6.0, 9.9, 15.0, 21.1, 28.0))
    getRequirements(spec=ThreePlusThreeSpec)
}

\keyword{methods}

