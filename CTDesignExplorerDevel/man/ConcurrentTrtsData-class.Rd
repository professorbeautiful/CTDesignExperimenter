\name{ConcurrentTrtsData-class}
\Rdversion{1.1}
\docType{class}
\alias{ConcurrentTrtsData-class}

\title{Class "ConcurrentTrtsData"}
\description{
    An S4 class, representing data objects that are obtained from a set of concurrent treatments.
}

\section{Objects from the Class}{
    Objects can be created by calls of the form \code{new("ConcurrentTrtsData", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{TrtAllos}:}{
       Object of class \code{"data.frame"}, containing concurrent treatments' allocation 
       information, which may include treatments' names, assigned doses, assigned dose levels, 
       routes, treatments' starting times, and treatments' ending times.  }
    \item{\code{Outcomes}:}{
       Object of class \code{"numeric"}, patient outcomes from the concurrent treatments.  }
    \item{\code{TimesToOutcomes}:}{
       Object of class \code{"\linkS4class{OptionalNumeric}"},a vector of times to patient 
       late-onset outcomes, which is NULL if outcomes are early-onset.  }
  }
}

\section{Methods}{
No methods defined with class "ConcurrentTrtsData" in the signature.
}

\seealso{
	\code{\linkS4class{PatData}},
    \code{\linkS4class{CTData}},
}

\examples{
showClass("ConcurrentTrtsData")
}

\keyword{classes}
