\name{OutcomeModelSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{OutcomeModelSpecifier-class}

\title{Class "OutcomeModelSpecifier"  }
\description{	 
  An S4 class union, whose member classes represent specifications for different outcome models.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
  \describe{
    \item{check1SetRequirements}{
        \code{signature(designSpec = "DesignSpecifier", 
        popModelSpec = "OptionalPopModelSpecifier", outcomeModelSpec = "OutcomeModelSpecifier", 
        evalSpec = "EvalSpecifier")}: checks whether a design, population model, outcome model and
        evaluation criterion can work together.  }
    \item{generatePatsOutcomes}{
        \code{signature(outcomeModelSpec = "OutcomeModelSpecifier", patsIndices = "numeric", 
        currentCTData = "CTData")}: generates outcomes for a specified group of patients.  }
    \item{sim1CT}{
        \code{signature(designSpec = "DesignSpecifier", 
        popModelSpec = "OptionalPopModelSpecifier", outcomeModelSpec = "OutcomeModelSpecifier")}:
        simulates a single replication of a clinical trial.  }
	 }
}

\examples{
showClass("OutcomeModelSpecifier")
}

\keyword{classes}
