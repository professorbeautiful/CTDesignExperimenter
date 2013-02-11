\name{APlusBSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{APlusBSpecifier-class}

\title{Class "APlusBSpecifier" }

\description{An S4 class, representing the specifications for "A+B" with dose de-escalation 
    designs.}
    
\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("APlusBSpecifier", ...)}. 
}

\section{Slots}{
  \describe{
    \item{\code{A}:}{Object of class \code{"numeric"}, the initial cohort size. }
    \item{\code{B}:}{Object of class \code{"numeric"}, the additional cohort size. }
    \item{\code{C}, \code{D}, \code{E}:}{Objects of class \code{"numeric"}, binary toxicity (e.g. dose-limiting
        toxicity; DLT) counts associated with stopping the trial and dose assignment for the next
         group of patients. }
    \item{\code{TierDoses}:}{Object of class \code{"numeric"}, a vector of selected doses for the testing. }
  }
}

\section{Extends}{
Class \code{"\linkS4class{DesignSpecifier}"}, directly.
}

\section{Methods}{
  \describe{
    \item{allocateTrts}{
        \code{signature(designSpec = "APlusBSpecifier", 
        currentCTData = "CTData", currentGlobalTime = "numeric", patsIndices = "numeric")}: 
        Assigns a dose of a testing treatment to the specified group of patients (current 
        patient cohort which has size either of \code{A} or \code{B}), and creates a list of 
        two new actions whose executions trigger the calls to \code{\link{generatePatsOutcomes}}
        and \code{\link{checkStoppingRule}} respectively.  } 
    \item{checkStoppingRule}{
        \code{signature(designSpec = "APlusBSpecifier", currentCTData = "CTData", 
        currentGlobalTime = "numeric")}: checks the stopping rule, and if the trial is to stop,draws the 
        conclusion on the recommended Phase II dose,otherwise creates a list of two new actions 
        whose executions trigger the calls to \code{link{generatePatsBaseChars}} and 
        \code{link{allocateTrts}} respectively.  }
    \item{generateInitialActions}{
        \code{signature(designSpec = "APlusBSpecifier")}: generates a list of two initial actions
        whose executions trigger the calls to \code{\link{generatePatsBaseChars}} and 
        \code{\link{allocateTrts}} respectively.  }
    \item{getProvisions}{
        \code{signature(spec = "APlusBSpecifier")}: gets a list of the names of items that 
         a "A+B" with dose de-escalation design provides.  }
    \item{getRequirements}{
        \code{signature(spec = "APlusBSpecifier")}: gets a list of the names 
         of items that a "A+B" with dose de-escalation design requires. }
	 }
}

\references{Lin, Y., Shih, W. J. 2001 Statistical Properties of the Traditional Algorithm-based
    designs for Phase I Cancer Clinical Trials, \emph{Biostatistics}, \bold{2}, 203--215 }

\examples{
showClass("APlusBSpecifier")
}

\keyword{classes}
