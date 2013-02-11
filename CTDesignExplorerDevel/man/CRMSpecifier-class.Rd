\name{CRMSpecifier-class}
\Rdversion{1.1}
\docType{class}
\alias{CRMSpecifier-class}

\title{Class "CRMSpecifier"  }

\description{ An S4 class, representing specifications for CRM designs.  }

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CRMSpecifier", ...)}.  }

\section{Slots}{
  \describe{
    \item{\code{InitialProbGuesses}:}{
        Object of class \code{"numeric"}, a vector of initial toxicity probability guesses of 
        tier doses in ascending order.  }
    \item{\code{TierDoses}:}{
        Object of class \code{"numeric"}, a vector of doses of a treatment for testing.  }
    \item{\code{TargetProb}:}{
        Object of class \code{"numeric"}, the target toxicity probability  }
    \item{\code{SampleSize}:}{
        Object of class \code{"numeric"}, the number of patients to enroll in a clinical trial.  }
    \item{\code{StartingDoseLevel}:}{
        Object of class \code{"\linkS4class{OptionalNumeric}"}, the starting dose level. Its 
        value is NULL in the two-stage CRM.  }
    \item{\code{InitialStageDoseLevels}:}{
        Object of class \code{"\linkS4class{OptionalNumeric}"}, a non-decreasing sequence of 
        dose levels of length \code{SampleSize} for the intial stage in two-stage CRM. Its
        value is NULL in the one-stage CRM.  }
    \item{\code{NPatsPerCohort}:}{
        Object of class \code{"numeric"}, the number of patients per cohort. Default is 1.  }
    \item{\code{EscalationRestriction}:}{
        Object of class \code{"logical"} containing information on whether or not applying dose 
        escalation restriction when considering the dose for the next patient cohort. If 
        \code{EscalationRestriction} = \code{FALSE}, the assigned dose will be totally 
        model-based. Otherwise, design will avoid (1) skipping doses in escalation, and 
        (2) incoherent escalation, which refers to escalation immediately after a toxicity outcome 
        when \code{NPatsPerCohort} = 1, and escalation immediately after the observed proportion of toxicity in the last cohort 
        is larger than  or equal to the target when \code{NPatsPerCohort} > 1.  }
    \item{\code{OutcomeModelType}:}{
        Object of class \code{"character"}, the types of outcome model. Currently,
        we have implemented two types of outcome models: \code{Exponential}, and \code{Logit}. 
        Default is \code{Exponential}.  }
    \item{\code{InterceptLogitModel}:}{
        Object of class \code{"numeric"}, the intercept of a logit model. Default is 3. If 
        the outcome model type is \code{Exponential}, this slot will not be used in the 
        simulation.  }
    \item{\code{StandardDeviationPrior}:}{
        Object of class \code{"numeric"}, the standard deviation of normal prior. Default is 
        1.34.  }
  }
}

\section{Extends}{
    Class \code{"\linkS4class{DesignSpecifier}"}, directly.  }
    
\section{Methods}{
  \describe{
    \item{allocateTrts}{
        \code{signature(designSpec = "CRMSpecifier", currentCTData ="CTData", 
        currentGlobalTime = "numeric", patsIndices = "numeric")}: Assigns a dose of 
        a testing treatment to the specified group of patients (current patient cohort), and 
        creates a list of two new actions whose executions trigger the calls to 
        \code{\link{generatePatsOutcomes}} and \code{\link{checkStoppingRule}} respectively.  }
    \item{checkStoppingRule}{
        \code{signature(designSpec = "CRMSpecifier", currentCTData = "CTData", 
        currentGlobalTime = "numeric")}: checks the stopping rule, and if the trial is to stop,
        draws the conclusion on the recommended Phase II dose and dose level ,otherwise creates a 
        list of two or three (if at the initial stage of a clinical trial with two-stage CRM
        design) new actions. See \link{checkStoppingRule} for details.  }
    \item{checkSwitchingStageRule}{
       \code{signature(designSpec = "CRMSpecifier", currentCTData = "CTData", 
       currentGlobalTime = "numeric")}: checks whether there is any binary toxicity outcome, and 
       if any, adds the current time as the time when switching from the initial stage to the 
       second stage to the temporary data repository within simulation.  }
    \item{generateInitialActions}{
        \code{signature(designSpec = "CRMSpecifier")}: creates a list of two initial actions 
        whose executions trigger the calls to \code{\link{generatePatsBaseChars}} and 
        \code{\link{allocateTrts}} respectively.  }
    \item{getProvisions}{
        \code{signature(spec = "CRMSpecifier")}: gets a list of names of the items that a CRM 
        design provides.  }
    \item{getRequirements}{
        \code{signature(spec = "CRMSpecifier")}: gets a list of names of the items that a CRM 
        design requires.  }
	}
}

\note{
    The \code{Exponential} model is specified as: \eqn{P=d^{exp(beta)}}, while the \code{Logit} 
    model is specified as: \eqn{logit(P)=alpha+exp(beta)*d}. In those two types of models, 
    \eqn{P} stands for the toxicity probability; \eqn{d} is the scaled doses; and \eqn{beta} 
    is the free parameter. \eqn{alpha} is the known intercept in the \code{Logit} model. The 
    prior on \eqn{beta} is assumed to be normal with mean 0. The current available estimation 
    method for \eqn{beta} is Bayesian, and we will implement the maximum likelihood estimation 
    method soon.  }
    
\references{ 
   O'Quigley, J. O., Pepe, M., and Fisher, L. 1990 Continual Reassessment Method: A Practical 
   Design for Phase I Clinical Trials in Cancer. \emph{Biometrics}, \bold{46}, 33--48.
   
   Cheung, Y. K. 2005 Coherence Principles in Dose-finding Studies. \emph{Biometrika}, \bold{92}, 
   863--873.
}

\examples{
showClass("CRMSpecifier")
}
\keyword{classes}
