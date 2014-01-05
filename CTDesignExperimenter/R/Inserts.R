cat("======== Inserts.R  ================\n")



#####
#' Event
setClass("Event",    ### Like a Variable
         slots=list(name="character", description="character"))
### Should there be  a time-to-event name?

#####
#' Class EventGenerator
setClass("EventGenerator",    ### Like a VariableGenerator
         contains="Specifier",
         slots=list(
           outputEvent="Event"
           ### this will be the Event caused.
           , conditionCode="function" 
           ### Arguments are the requirements,
           ### which are VariableValues.
           ### Calculates boolean; if TRUE, places the Event on the queue, 
         )
         ,
         validity=function(object) { # has to be "object"
           ### TODO: Check if the return value is boolean
           return(TRUE)
         }
)
#####
setClassUnion("Insert", c("VariableGenerator", "EventGenerator"))
#setClass("InsertVariable", contains=c("Insert", "VariableGenerator"))
#setClass("InsertEvent", contains=c("Insert", "EventGenerator"))

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

setClass("EventAtTime", contains="Event", slots=list(eventTime="numeric"))
###  we are not currently using EventAtTime.

setClassUnion("Action", c("Event", "Variable"))

setClassUnion("Insert", c("EventGenerator", "VariableGenerator"))



setClass("PatientAttribute",      contains="VariableGenerator")
setClass("EligibilityCriterion",  contains="VariableGenerator")
setClass("ScheduleTreatment",     contains="EventGenerator")
setClass("OffStudyCriterion",     contains="VariableGenerator")
setClass("ModificationRule",      contains="EventGenerator")
setClass("PatientSummary",        contains="VariableGenerator")
setClass("StoppingCriterion",     contains="VariableGenerator")
setClass("TrialSummary",          contains="VariableGenerator")

#' ListOfInserts
#' 
#' A list of Insert objects.
#' Generally the parameters are the defaults.
#' When a new Scenario is defined, the parameters may be changed.
setClass("ListOfInserts", contains="list")
validity_ListOfInserts = function(object) {
  if(length(object)==0) return(TRUE)
  whichAreInserts = sapply(object, is, "Insert")
  if(all(whichAreInserts)) return(TRUE)
  return(paste("ERROR in ListOfInserts: ", which(!whichAreInserts)))
}
setValidity(Class="ListOfInserts", validity_ListOfInserts)

##### EligibilityCriteria ##### 
setValidity(Class="EligibilityCriterion", 
         method=function(object) {
           provision = slot(object, name="provisions")
           if(!is(provisions, "Variables"))
             return("eligibilityCriterion provision is not a Variable")
           if(identical(slot(object, provision)@checkDataType, is.logical))
             return("checkDataType should be is.logical")
           return(TRUE)
         }
)
 
vg_age = as(Class="PatientAttribute",
            VariableGenerator(parameters=list(ageMean=50, ageSD=10),
                              generatorCode=function(){
                                rnorm(1, ageMean, ageSD)
                              },
                              provisions=v_ageVariable,
            ))
ec_age = as(Class="EligibilityCriterion", VariableGenerator( parameters=list(cutoff=50),
                  requirements=VariableList(v_ageVariable),
                  outputVariable=Variable(name="isOldEnough",
                                          description="Patient is old enough",
                                          checkDataType=is.logical),
                  generatorCode=function()  { age >= cutoff}
))  #### OK.
v_liverVariable = Variable(name="liverFunction", desc="Liver function", 
                           gitAction="none",
                           checkDataType=function(x) is.numeric(x))
vg_liver = as(Class="PatientAttribute",
            VariableGenerator(parameters=list(liverMean=1, liverSD=0.2),
                              generatorCode=function(){
                                rnorm(1, liverMean, liverSD)
                              },
                              provisions=v_liverVariable
            ))
ec_liver = as(Class="EligibilityCriterion", 
              VariableGenerator( parameters=list(cutoff=1.5),
                                 requirements=VariableList(v_liverVariable),
                                 outputVariable=Variable(name="liverOK",
                                                         description="Patient has sufficient liver function.",
                                                         checkDataType=is.logical),
                                 generatorCode=function()  { liverFunction <= cutoff}
              ))  #### OK.

#  TODO 
# st_oneDose = as(Class="ScheduleTreatment",
#                 EventGenerator(parameters=list(theDose),
#                                generatorCode=))
