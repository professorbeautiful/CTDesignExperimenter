\name{getOtherArgs-methods}
\docType{methods}
\alias{getOtherArgs-methods}
\alias{getOtherArgs,Action-method}
\alias{getOtherArgs}

\title{Method "getOtherArgs"  }

\description{
    assigns the "other" arguments' values to their names in the parent frame where the action 
    is executed.
}

\section{Methods}{
\describe{
    \item{\code{signature(action = "Action")}}{
    assigns the "other" arguments' values to their names in the parent frame where the action 
    is executed. "Other" arguments refers to the arguments of the action method whose 
    values are obtained in the environment from which the action is created.   }
}}

\seealso{
  \linkS4class{Action}
}

\keyword{methods}
