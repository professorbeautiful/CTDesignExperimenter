\name{generateBaseChar-methods}
\docType{methods}
\alias{generateBaseChar-methods}
\alias{generateBaseChar,BaseCharModelSpecifier-method}
\alias{generateBaseChar}

\title{ Method "generateBaseChar"  }
\description{
   generates a single baseline characteristic for a patient.
}

\section{Methods}{
\describe{
    \item{\code{signature(baseCharModelSpec = "BaseCharModelSpecifier")}}{
        generates the baseline characteristic from the specified R function in 
        \code{baseCharModelSpec}.  }
}}

\note{
    This method is an internal method, which is called within the method \link{generateBaseChars}.
    To model the total joint distribution of a patient's baseline characteristics, we model 
    each baseline characteristic using sequential conditional distributions. 
}

\keyword{methods}
