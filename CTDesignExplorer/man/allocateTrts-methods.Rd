\name{allocateTrts-methods}
\docType{methods}
\alias{allocateTrts-methods}
\alias{allocateTrts,APlusBSpecifier,CTData,numeric,numeric-method}
\alias{allocateTrts,CRMSpecifier,CTData,numeric,numeric-method}
\alias{allocateTrts}

\title{Methods: "allocateTrts"}
\description{
 Allocates a set of concurrent treatments (the number of concurrent treatments in a set can be one) to a specified group of patients,
 and also creates a list of new actions.
}

\section{Methods}{
\describe{
  \item{\code{signature(designSpec = "APlusBSpecifier",currentCTData = "CTData",currentGlobalTime = 
  "numeric",patsIndices = "numeric")}}{
      Assigns a dose of a testing treatment to the specified group of patients (current 
      patient cohort which has size either of \code{A} or \code{B}), and creates a list of 
      two new actions whose executions trigger the calls to \code{\link{generatePatsOutcomes}}
      and \code{\link{checkStoppingRule}} respectively.  }   
  \item{\code{signature (designSpec = "CRMSpecifier", currentCTData = "CTData", currentGlobalTime = 
  "numeric", patsIndices = "numeric")}}{ 
      Assigns a dose of a testing treatment to the specified group of patients (current 
      patient cohort), and creates a list of two new actions whose executions trigger the 
      calls to \code{\link{generatePatsOutcomes}} and \code{\link{checkStoppingRule}} 
      respectively.  }
}}

\note{
    The argument \code{patsIndices} refers to the order numbers in which patients are enrolled
    to a clinical trial. If several patients, say \eqn{n}, happen to be enrolled simultaneously, suppose
    the last previous patient index is \eqn{i}, then assign indices \eqn{i+1, i+2,...,i+n} casually
    to those patients. The information for a treatment may include treatment name, dose,dose level,
    unit, route, starting time and ending time.  }
    
\keyword{methods}
