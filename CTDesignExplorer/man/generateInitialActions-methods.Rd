\name{generateInitialActions-methods}
\docType{methods}
\alias{generateInitialActions-methods}
\alias{generateInitialActions,APlusBSpecifier-method}
\alias{generateInitialActions,CRMSpecifier-method}
\alias{generateInitialActions}

\title{ Methods "generateInitialActions"  }
\description{
    generates a list of initial actions in an action queue.
}

\section{Methods}{
\describe{
    \item{\code{signature(designSpec = "APlusBSpecifier")}}{
        creates a list of two initial actions whose executions trigger the calls to 
        \code{\link{generatePatsBaseChars}} and \code{\link{allocateTrts}} respectively. }

    \item{\code{signature(designSpec = "CRMSpecifier")}}{ 
        creates a list of two initial actions 
        whose executions trigger the calls to \code{\link{generatePatsBaseChars}} and 
        \code{\link{allocateTrts}} respectively.  }
}}

\keyword{methods}
