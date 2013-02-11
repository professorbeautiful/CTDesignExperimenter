\name{checkSwitchingStageRule-methods}
\docType{methods}
\alias{checkSwitchingStageRule-methods}
\alias{checkSwitchingStageRule,CRMSpecifier,CTData,numeric-method}
\alias{checkSwitchingStageRule}

\title{ Methods "checkSwitchingStageRule" }
\description{
   checks the switching-stage rule in a clinical trial with multiple stages; if the 
   switching-stage rule decides to switch from the current stage to the next stage, 
   adds the current time as \code{SwitchingStageTime} to the temporary data repository within 
   simulation.
}

\section{Methods}{
\describe{
    \item{\code{signature(designSpec = "CRMSpecifier", currentCTData = "CTData", 
    currentGlobalTime = "numeric")}}{
        checks if there is any binary toxicity outcome; adds the current time as 
        \code{SwitchingStageTime} if any to the temporary data repository within simulation.  }
}}

\keyword{methods}

