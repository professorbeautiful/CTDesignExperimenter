\name{checkStoppingRule-methods}
\docType{methods}
\alias{checkStoppingRule-methods}
\alias{checkStoppingRule,APlusBSpecifier,CTData,numeric-method}
\alias{checkStoppingRule,CRMSpecifier,CTData,numeric-method}
\alias{checkStoppingRule}

\title{ Methods "checkStoppingRule" }
\description{
    checks the stopping rule of a clinical trial, and creates a list of new actions if a clinical
    trial is to continue, otherwise adds new data such as conclusions from a clinical trial.
}

\section{Methods}{
\describe{
  \item{\code{signature(designSpec = "APlusBSpecifier", currentCTData = "CTData", 
      currentGlobalTime = "numeric")}}{
          checks the stopping rule, and if the trial is to stop,draws the conclusion on the 
          recommended Phase II dose,otherwise creates a list of two new actions whose executions
          trigger the calls to \code{link{generatePatsBaseChars}} and \code{link{allocateTrts}}
          respectively.  }
  \item{\code{signature(designSpec = "CRMSpecifier", currentCTData = "CTData", 
      currentGlobalTime = "numeric")}}{
         checks the stopping rule, and if the trial is to stop,draws the conclusion on the 
         recommended Phase II dose and dose level ,otherwise creates a list of two or three (if at
         the initial stage of a clinical trial with two-stage CRM design) new actions. The
         executions of the new actions trigger the calls in temporal order to: (1) 
         \code{\link{checkSwitchingStageRule}}(if in the initial stage of a two-stage CRM trial),
         (2)\code{link{generatePatsBaseChars}},and (3)\code{link{allocateTrts}}.   }
}}

\keyword{methods}

