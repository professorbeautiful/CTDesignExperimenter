\name{EvalSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{EvalSpecifier-class}

\title{Class "EvalSpecifier"  }

\description{	 
    An S4 class union, whose memeber classes represent the specifications for different evaluation
    criteria  }
    
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{check1SetRequirements}{
        \code{signature(designSpec = "DesignSpecifier", popModelSpec = 
        "OptionalPopModelSpecifier", outcomeModelSpec = "OutcomeModelSpecifier", 
        evalSpec = "EvalSpecifier")}: checks whether a design, population model, outcome model
        and evaluation criterion can work together.  }
	 }
}

\examples{
showClass("EvalSpecifier")
}

\keyword{classes}
