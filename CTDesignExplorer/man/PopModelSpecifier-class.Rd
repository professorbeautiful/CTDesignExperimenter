\name{PopModelSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{PopModelSpecifier-class}

\title{Class "PopModelSpecifier"  }
\description{	
    An S4 class, representing the specification for a population model.
}

\section{Objects from the Class}{
   Objects can be created by calls of the form \code{new("PopModelSpecifier", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{PopModelSpec}:}{Object of class \code{"list"}, a list of model specifications for
    different baseline characteristics.  }
  }
}

\section{Extends}{
Class \code{"\linkS4class{OptionalPopModelSpecifier}"}, directly.
}
\section{Methods}{
  \describe{
    \item{generateBaseChars}{
        \code{signature(popModelSpec = "PopModelSpecifier")}: generate baseline characteristics for
        a patient.  }
	 }
}

\examples{
showClass("PopModelSpecifier")
}

\keyword{classes}
