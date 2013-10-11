\name{OptionalPopModelSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{OptionalPopModelSpecifier-class}

\title{Class "OptionalPopModelSpecifier"   }
\description{	 
    An S4 class union, whose member classes are class \code{"NULL"} and class 
    \code{"\linkS4class{PopModelSpecifier}"}.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{check1SetRequirements}{
        \code{signature(designSpec = "DesignSpecifier", popModelSpec = "OptionalPopModelSpecifier", 
         outcomeModelSpec = "OutcomeModelSpecifier", evalSpec = "EvalSpecifier")}: checks
         whether a design, population model, outcome model and evaluation criterion can work
         together.  }
    \item{generatePatsBaseChars}{
        \code{signature(popModelSpec = "OptionalPopModelSpecifier", nPats = "numeric", 
        currentCTData = "CTData")}: generates baseline characteristics for a specified group
        of patients.  }
    \item{getProvisions}{
        \code{signature(spec = "OptionalPopModelSpecifier")}: gets a list of names of the items
        that \code{spec} provides.  }
    \item{sim1CT}{
        \code{signature(designSpec = "DesignSpecifier", 
        popModelSpec = "OptionalPopModelSpecifier", outcomeModelSpec = "OutcomeModelSpecifier")}:
        simulates a single replication of a clinical trial.  }
	 }
}

\examples{
showClass("OptionalPopModelSpecifier")
}
\keyword{classes}
