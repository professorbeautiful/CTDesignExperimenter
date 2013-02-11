\name{checkRequirements-methods}
\docType{methods}
\alias{checkRequirements-methods}
\alias{checkRequirements,list,list,list,list-method}
\alias{checkRequirements}

\title{ Method: "checkRequirements"  }
\description{
    checks what designs, population models, outcome models and evaluation criteria can 
    satisfy the requirements of each other. (i.e. interoperability).
}

\section{Methods}{
\describe{
    \item{\code{signature(designSpecs = "list", popModelSpecs = "list",
    outcomeModelSpecs = "list", evalSpecs = "list")}}{ 
    checks what designs, population models, outcome models and evaluation criteria can 
    satisfy the requirements of each other.}
}}

\note{
    \code{designSpecs} is a list of specifications for different designs; \code{popModelSpecs}
    is a list of specifications for different population models; \code{outcomeModelSpecs} is 
    a list of specifications for different outcome models; and \code{evalSpecs} is a list of
    specifications for different evaluation criteria.
    We currently use name matching mechanisms to check the interoperability
    among objects. 
}

\keyword{methods}

