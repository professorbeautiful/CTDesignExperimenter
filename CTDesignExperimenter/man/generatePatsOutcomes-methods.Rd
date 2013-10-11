\name{generatePatsOutcomes-methods}
\docType{methods}
\alias{generatePatsOutcomes-methods}
\alias{generatePatsOutcomes,OutcomeModelSpecifier,numeric,CTData-method}
\alias{generatePatsOutcomes}

\title{ Method "generatePatsOutcomes"  }

\description{
    generates outcome(s) for a specified group of patients.
}
\section{Methods}{
\describe{

    \item{\code{signature(outcomeModelSpec = "OutcomeModelSpecifier", patsIndices = "numeric", 
    currentCTData = "CTData")}}{ generates outcomes for a specified group of patients.  }
}}

\note{
    The argument \code{patsIndices} refers to the order numbers in which patients are enrolled
    to a clinical trial. If several patients, say \eqn{n}, happen to be enrolled simultaneously, suppose
    the last previous patient index is \eqn{i}, then assign indices \eqn{i+1, i+2,...,i+n} casually
    to those patients. 
}

\seealso{
  \link{generateOutcomes}
}

\keyword{methods}

