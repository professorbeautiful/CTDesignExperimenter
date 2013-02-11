\name{Phase2BryantDaySpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{Phase2BryantDaySpecifier-class}

\title{Class "Phase2BryantDaySpecifier"  }
\description{	 
   An S4 class, representing the specification for a Phase II Bryant and Day design.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Phase2BryantDaySpecifier", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{N1Pats}:}{
        Object of class \code{"numeric"}, the number of patients to enroll in 
        the first stage of a Phase II Bryant and Day design.  }
    \item{\code{NPats}:}{
        Object of class \code{"numeric"}, the total number of patients to enroll in a 
        a Phase II Bryant and Day design.  }
    \item{\code{Efficacy1LL}:}{
        Object of class \code{"numeric"}, the lower limit of the number of efficacy responses
        to observe in the first stage for swtiching to the second stage.   }
    \item{\code{EfficacyLL}:}{
        Object of class \code{"numeric"}, the lower limit of the total number of efficacy responses
        to observe for a drug successfully passing a Phase II trial.}
    \item{\code{NonTox1LL}:}{
        Object of class \code{"numeric"}, the lower limit of the number of non-toxicity responses
        to observe in the first stage for swtiching to the second stage.   }
    \item{\code{NonToxLL}:}{
        Object of class \code{"numeric"}, the lower limit of the total number of non-toxicity responses
        to observe for a drug successfully passing a Phase II trial.}
  }
}

\section{Extends}{
Class \code{"\linkS4class{DesignSpecifier}"}, directly.
}

\section{Methods}{
No methods defined with class "Phase2BryantDaySpecifier" in the signature.
}

\references{ 
    Bryant J. and Day R. 1995 Incorporating Toxicity Considerations Into the Design 
    of Two-Stage Phase II Clinical Trials, \emph{Biometrics}, \bold{51}, 1372--1383. 
 }

\seealso{
	\linkS4class{EvalProbSuccPhase2Specifier}
}

\examples{
showClass("Phase2BryantDaySpecifier")
}

\keyword{classes}
