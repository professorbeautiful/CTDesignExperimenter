\name{instantiateS4Object}
\alias{instantiateS4Object}
\title{
    Function "instantiateS4Object"
}
\description{
    An R function for instantiating an object from an S4 class.
}

\usage{
instantiateS4Object(className, slots)
}

\arguments{
  \item{className}{
    The name of an S4 class.
}
  \item{slots}{
    A named list with the names corresponding to the slot names.
}
}

\value{
    It returns an S4 object.
}

\examples{
# Specify a "3+3" with dose de-escalation design,
# that is to say, instantiate an S4 object from the class "APlusBSpecifier"
instantiateS4Object(
    className="APlusBSpecifier",
 	slots=list(A=3,B=3,C=1,D=1,E=1,TierDoses=c(3.0,6.0,9.9,15.0,21.1,28.0))
)
}


\keyword{ utilities }

