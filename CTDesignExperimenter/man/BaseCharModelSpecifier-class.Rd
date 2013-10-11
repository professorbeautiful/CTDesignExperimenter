\name{BaseCharModelSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{BaseCharModelSpecifier-class}

\title{Class "BaseCharModelSpecifier"  }

\description{An S4 class, representing specifications for baseline characteristic models}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BaseCharModelSpecifier", ...)}. 
}

\section{Slots}{
  \describe{
    \item{\code{BaseCharName}:}{
        Object of class \code{"character"}, containing a baseline characteristic name. }
    \item{\code{ConditionBaseCharNames}:}{
        Object of class \code{"\linkS4class{OptionalCharacter}"}, containing 
        a vector of names of conditioning baseline characteristics.}
    \item{\code{RGenFun}:}{
        Object of class \code{"character"}, containing a string of an R function for 
        generating the baseline characteristic. }
  }
}

\section{Methods}{
  \describe{
    \item{generateBaseChar}{
        \code{signature(baseCharModelSpec = "BaseCharModelSpecifier")}: generates
        the baseline characteristic from the specified R function in \code{baseCharModelSpec}.  }
    \item{getProvisions}{
        \code{signature(spec = "BaseCharModelSpecifier")}: gets the name of the baseline 
        characteristic that \code{spec} provides. }
    \item{getRequirements}{
        \code{signature(spec = "BaseCharModelSpecifier")}: gets the names of the conditioning 
        baseline characteristics that \code{spec} has.  }
	 }
}

\seealso{
	 \code{\linkS4class{PopModelSpecifier}}
}

\examples{
showClass("BaseCharModelSpecifier")
}

\keyword{classes}
