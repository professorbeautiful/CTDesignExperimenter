\name{evalDesign-methods}
\docType{methods}
\alias{evalDesign-methods}
\alias{evalDesign,EvalNPatsAtEachDoseSpecifier,list-method}
\alias{evalDesign,EvalNToxsAtEachDoseSpecifier,list-method}
\alias{evalDesign,EvalNToxsSpecifier,list-method}
\alias{evalDesign,EvalPctPatsAtEachDoseSpecifier,list-method}
\alias{evalDesign,EvalProbRP2DAtEachDoseSpecifier,list-method}
\alias{evalDesign,EvalRP2DSpecifier,list-method}
\alias{evalDesign,EvalSampleSizeSpecifier,list-method}
\alias{evalDesign,EvalToxRateSpecifier,list-method}
\alias{evalDesign}

\title{ Methods "evalDesign"  }

\description{
    evaluates a clinical trial (CT) design under a certain criterion.
}

\section{Methods}{
\describe{

\item{\code{signature(evalSpec = "EvalNPatsAtEachDoseSpecifier", simCTsData = "list")}}{
    evaluates the number of patients allocated at each dose.  }

\item{\code{signature(evalSpec = "EvalNToxsAtEachDoseSpecifier", simCTsData = "list")}}{
    evaluates the number of binary toxicity outcomes observed at each dose.  }

\item{\code{signature(evalSpec = "EvalNToxsSpecifier", simCTsData = "list")}}{ 
    evaluates the total number of observed binary toxicity outcomes. It returns a list of two elements: 
    Summary (mean and variance) and a vector of total number of observed binary toxicity outcomes. }

\item{\code{signature(evalSpec = "EvalPctPatsAtEachDoseSpecifier", simCTsData = "list")}}{ 
    evaluates the percentage of patients allocated at each dose.  }

\item{\code{signature(evalSpec = "EvalProbRP2DAtEachDoseSpecifier", simCTsData = "list")}}{ 
    evalates the probability of each dose being chosen as the recommended Phase II dose (RP2D).  }

\item{\code{signature(evalSpec = "EvalRP2DSpecifier", simCTsData = "list")}}{ 
    evaluates the chosen RP2Ds. It returns a list of two elements: Summary (mean, variance
    number of \code{NA}s) and a vector of chosen RP2Ds.  }

\item{\code{signature(evalSpec = "EvalSampleSizeSpecifier", simCTsData = "list")}}{ 
    evaluates the sample sizes.It returns a list of two elements: 
    Summary (mean and variance) and a vector of sample sizes.  }

\item{\code{signature(evalSpec = "EvalToxRateSpecifier", simCTsData = "list")}}{ 
    evaluates the average toxicity rate.  }
}}

\keyword{methods}

