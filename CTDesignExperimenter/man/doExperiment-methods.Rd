\name{doExperiment-methods}
\docType{methods}
\alias{doExperiment-methods}
\alias{doExperiment,list,list,list,list,numeric,OptionalNumeric,OptionalCharacter,logical-method}
\alias{doExperiment}

\title{Method "doExperiment"  }
\description{
   performs a simulation experiment and returns evaluation results.
}

\section{Methods}{
\describe{
    \item{\code{signature(designSpecs = "list", popModelSpecs = "list", 
    outcomeModelSpecs = "list", evalSpecs = "list", nReps = "numeric", seed = "OptionalNumeric", 
    simDataDir = "OptionalCharacter",userInput="logical")}}{
        checks what designs, population models, outcome models and evaluation criteria can work 
        (i.e. interoperate) with each other. Among those interoperable objects, each combination 
        of a design, population model and outcome model constitutes a scenario. For each scenario,
        it runs \code{nReps} clinical trial (CT) simulations, and the simulated CT data are 
        evaluated by the corresponding criteria. After simulation and evaluation are done for all 
        scenarios, it returns the evaluation results.  }
}}

\note{
    \code{designSpecs} is a list of specifications for different designs, \code{popModelSpecs} is
    a list of specifications for different population models, \code{outcomeModelSpecs} is a list
    of specifications for different outcome models, and \code{evalSpecs} is a list of specifications
    for different evaluation criteria. If no population model is specified, then \code{popModelSpecs = 
    list(NULL)}.
    After checking the interoperability among designs, population models, outcome models and evaluation 
    criteria, it will print out the valid scenarios. If \code{userInput=TRUE},it will asks users for the
    permission to proceed.
    Users can replicate results by specifying the seed for 
    simulation, and they can also save simulated data to a specified directory. The directory should
    ended with a solidus. The names of the saved simulated CT data are in the format of "SimCTsData_Si.RData", 
    where Si refers to scenario i.
}

\seealso{
    \code{\link{sim1CT}}   
}    
        
\keyword{methods}

