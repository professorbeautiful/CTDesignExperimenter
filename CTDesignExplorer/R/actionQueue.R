cat("======== actionQueue.R  ================\n")



## Method: generatePatsOutcomes
# The calling string for this method is an action slot in an action 
# patsIndices are the enrollment order numbers of patients, which are the same as the element order numbers of PatsData object  
setGeneric("generatePatsOutcomes",
           function(outcomeModelSpec,patsIndices,currentCTData) 
             standardGeneric("generatePatsOutcomes"))

setMethod("generatePatsOutcomes",signature(
  outcomeModelSpec="OutcomeModelSpecifier",
  patsIndices="numeric",
  currentCTData="CTData"),
          function(outcomeModelSpec,patsIndices,currentCTData){
            for ( i in patsIndices)
              currentCTData@PatsData[[i]]<-generateOutcomes(outcomeModelSpec=outcomeModelSpec,thisPatCurrentData=currentCTData@PatsData[[i]])
            return(list(NewCTData=currentCTData))
          }
) # It returns a list with one element for NewCTData

## Class:ConcurrentTrtsData

# This class represents data from a set of concurrent treatments for a patient
# The number of concurrent treatments is an integer >= 1
# TrtAllos is a data frame which may have columns for TrtName, Dose,Dose level, Unit, Route, StartTime and EndTime 
setClass("ConcurrentTrtsData",
         representation(TrtAllos="data.frame",Outcomes="numeric",TimesToOutcomes="OptionalNumeric"),
         prototype=list(TimesToOutcomes=NULL))

## Class: PatData
# This class represents data from a patient
# ConcurrentTrtsDataList is a list of data from different sets of concurrent treatments
# PatTimes may include enrollment time, off-CT time for a patient
setClass("PatData",
         representation(ID="OptionalNumeric",BaseChars="OptionalNumeric",ConcurrentTrtsDataList="list",
                        PatTimes="OptionalNumeric"),
         prototype=list(ID=NULL,BaseChars=NULL,PatTimes=NULL),
         validity=function(object){
           if (length(object@ConcurrentTrtsDataList)==0) TRUE
           else{
             if(! all(Check<-sapply(object@ConcurrentTrtsDataList, function(x) is(x,"ConcurrentTrtsData"))))
               stop("The following elements are not objects of the ConcurrentTrtsData class:","\n",paste(which(!Check), collapse=","),"\n" )
             else TRUE
           }
         }
)

## Class: CTData
# This class represents data from a single CT (clinical trial).
# CTTimes may include switching-stage time and stopping-CT time.
setClass("CTData",
         representation(PatsData="list",CTTimes="OptionalNumeric",Conclusions="NumericLogical"),
         prototype=list(CTTimes=NULL),
         validity=function(object){
           if (length(object@PatsData)==0) TRUE
           else{
             if(! all(Check<-sapply(object@PatsData, function(x) is(x,"PatData"))))
               stop("The following elements are not objects of the PatData class:","\n",paste(which(!Check), collapse=","),"\n" )
             else TRUE
           }
         }
)

### Classes and Methods for Action Queue
## Class: Action
# OtherArgs:the arguments whose values are obtained from the method call within which this action is generated; 
# OtherArgs is a named list 
# GlobalTime in an "Action" object refers to the time when the action method is called.
# In an action queue, all GlobalTime's should have the same reference time point.
# If no time specificBaseCharModelSpecifieration is available in the design specification, GlobalTime can be 1,2,3...for
# first action, second action, third action, etc in an action queue and when a new action is added to
# the queue, its GlobalTime is (GlobalTime of the last action in a queue + 1)
setClass("Action",representation(MethodCall="character",OtherArgs="list",GlobalTime="numeric"))

## Method: getOtherArgs
setGeneric("getOtherArgs",function(action) standardGeneric("getOtherArgs"))

setMethod("getOtherArgs",signature(action="Action"),
          function(action){
            OtherArgs <- action@OtherArgs 
            NArgs <- length(OtherArgs)
            if (NArgs != 0){
              for ( i in 1:length(OtherArgs))
                assign(names(OtherArgs)[i],OtherArgs[[i]],envir=parent.frame())
            }
          }
)

## Class: ActionQueue
# An Action Queque is in the ascending temproal order of dispatching actions
setClass("ActionQueue",representation(ActionQ="list"),
         validity=function(object){
           if (length(object@ActionQ)!=0){
             if(! all(Check<-sapply(object@ActionQ, function(x) is(x,"Action"))))
               stop("The following elements are not objects of the Action class:","\n",paste(which(!Check), collapse=","),"\n" )
             else TRUE
           }
           else TRUE
         }
)    

## Method: addAction
setGeneric("addAction",function(currentActionQ,newAction) standardGeneric("addAction"))

setMethod("addAction",signature(currentActionQ="ActionQueue",newAction="Action"),
          function(currentActionQ,newAction){
            # Number of Actions in the current action queue
            NActions <- length(currentActionQ@ActionQ)
            if (NActions == 0)
              currentActionQ@ActionQ <- c(currentActionQ@ActionQ, newAction)
            else { 
              CurrentGlobalTimes <- sapply(currentActionQ@ActionQ,function(x) x@GlobalTime)
              NewGlobalTime <- newAction@GlobalTime
              # New action will be the last one to be dispatched among the actions that have the same 
              # GlobalTime as newGlobalTime
              if (CurrentGlobalTimes[NActions] <= NewGlobalTime) currentActionQ@ActionQ <- c(currentActionQ@ActionQ, newAction)
              else{
                IndexAfterNewAction <- min((1:NActions)[CurrentGlobalTimes > NewGlobalTime])
                currentActionQ@ActionQ[IndexAfterNewAction:(NActions+1)] <- c(newAction, currentActionQ@ActionQ[IndexAfterNewAction:NActions])
              }
            }
            return(currentActionQ)
          }
) # It returns a new action queue
