cat("======== Inserts.R  ================\n")



#####
#' Event
setClass("Event",    ### Like a Variable
         contains="SwapItem",
         slots=list(
           timeToNextEvent="character", #expression
           who="character" #expression
           ))
### Should there be  a time-to-event name?

#####
#' Class EventGenerator
setClass("EventGenerator",    ### Like a VariableGenerator
         contains="VariableGenerator",
         slots=list(
           outputEvent="Event"
           ### this will be the Event caused.
           , conditionCode="function"
           ### Arguments are the requirements,
           ### which are VariableValues.
           ### Calculates boolean; if TRUE, places the Event on the queue, 
           , delayCode="function"
           ### returns the delta time until   
         )
         ,
         validity=function(object) { # has to be "object"
           ### TODO: Check if the return value is boolean
           return(TRUE)
         }
)
#####
setClassUnion("Insert", c("VariableGenerator", "EventGenerator"))

#' function EventGenerator
EventGenerator = function(parameters=list(), provisions, 
                          requirements=NULL,
                          outputEvent, generatorCode) {
  if(missing(provisions)) provisions=list(outputVariable)
  if(missing(outputVariable)) outputVariable=provisions
  eg = new("EventGenerator", 
           parameters=parameters,
           provisions=provisions,
           requirements=requirements,
           generatorCode=generatorCode,
           outputEvent=outputEvent)
  if(is(requirements, "Variable"))
    requirements=list(requirements)
  environment(eg@generatorCode) = new.env()
  if(length(parameters) > 0)
    environment(eg@generatorCode) = list2env(parameters, new.env())
  eg
}

# setClass("EventAtTime", contains="Event", slots=list(eventTime="numeric"))
###  we are not currently using EventAtTime.

setClassUnion("Action", c("Event", "Variable"))


##### EligibilityCriteria ##### 
# TODO: move this to VariableGenerator.
# setValidity(Class="EligibilityCriterion", 
#          method=function(object) {
#            provision = slot(object, name="provisions")
#            if(!is(provisions, "Variables"))
#              return("eligibilityCriterion provision is not a Variable")
#            if(identical(slot(object, provision)@checkDataType, is.logical))
#              return("checkDataType should be is.logical")
#            return(TRUE)
#          }
# )

rlognorm = function(n=1, Mean=50, CV=0.5) {
  uv = FromNormalToLognormal(mean=Mean, cv=CV)
  exp(rnorm(n=n, mean=uv["uStar"], sd=sqrt(uv["vStar"])))
}

