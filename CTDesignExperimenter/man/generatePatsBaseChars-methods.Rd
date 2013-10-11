\name{generatePatsBaseChars-methods}
\docType{methods}
\alias{generatePatsBaseChars-methods}
\alias{generatePatsBaseChars,OptionalPopModelSpecifier,numeric,CTData-method}
\alias{generatePatsBaseChars}

\title{ Method "generatePatsBaseChars"  }
\description{
   generates baseline characteristics for a specified number of patients.
}
\section{Methods}{
\describe{

\item{
    \code{signature(popModelSpec = "OptionalPopModelSpecifier", nPats = "numeric", 
    currentCTData = "CTData")}}{ 
        generates baseline characteristics for a specified number (\code{nPats}) of patients.
        If \code{popModelSpec} is NULL, generates a list of (\code{nPats}) empty objects of 
        \code{linkS4class{PatData}}.  }
}}

\seealso{
   \link{generateBaseChars}
}

\keyword{methods}

