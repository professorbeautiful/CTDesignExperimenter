cat("Inserts.R")

#####
setClass("Event",    ### Like a Variable
         slots=list(name="character", description="character"))
### Should here be  a time-to-event name?

setClass("EventGenerator",    ### Like a VariableGenerator
         contains="Specifier",
         slots=list(
           outputEvent="Event"
           ### this will be the Event caused.
           , generatorCode="function" 
           ### Arguments are the requirements,
           ### which are VariableValues.
           ### Calculates boolean; if TRUE, places the Event on the queue 
         )
         ,
         validity=function(object) { # has to be "object"
           ### TODO: Check if the return value is boolean
           return(TRUE)
         }
)

setValidity(Class="EligibilityCriterion", 
         method=function(object) {
           provision = slot(object, name="provisions")
           if(!is(provision, "Variable"))
             return("eligibilityCriterion provision is not a Variable")
           if(identical(slot(object, provision)@checkDataType, is.logical))
             return("checkDataType should be is.logical")
           return(TRUE)
         }
)

new("EligibilityCriterion", )