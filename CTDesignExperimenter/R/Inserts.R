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
           , generatorCode="function" 
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
#' function EventGenerator
EventGenerator = function(parameters=list(), provisions, 
                             requirements=NULL,
                             outputEvent, generatorCode) {
  if(missing(provisions)) provisions=list(outputVariable)
  if(missing(outputVariable)) outputVariable=provisions
  vg = new("VariableGenerator", 
           parameters=parameters,
           provisions=provisions,
           requirements=requirements,
           generatorCode=generatorCode,
           outputVariable=outputVariable)
  if(is(requirements, "Variable"))requirements=list(requirements)
  environment(vg@generatorCode) = new.env()
  if(length(parameters) > 0)
    environment(vg@generatorCode) = list2env(parameters, new.env())
  vg
}

setClassUnion("Action", c("Event", "Variable"))
setClassUnion("ActionGenerator", c("EventGenerator", "VariableGenerator"))


setClass("Insert", contains="Specifier")
setClass("InsertVariable", contains=c("Insert", "VariableGenerator"))
setClass("InsertEvent", contains=c("Insert", "EventGenerator"))


setClass("PatientAttribute",      contains="InsertVariable")
setClass("EligibilityCriterion",  contains="InsertVariable")
setClass("ScheduleTreatment",     contains="InsertEvent")
setClass("OffStudyCriterion",     contains="InsertVariable")
setClass("ModificationRule",      contains="InsertEvent")
setClass("PatientSummary",        contains="InsertVariable")
setClass("StoppingCriterion",     contains="InsertEvent")
setClass("TrialSummary",          contains="InsertVariable")

#' ListOfInserts
#' 
#' A list of Insert objects.
#' Generally the parameters are the defaults.
#' When a new Scenario is defined, the parameters may be changed.
setClass("ListOfInserts", contains="list",
         validity=function(object) {
           if(length(object)==0) return(TRUE)
           whichAreInserts = sapply(object, is, "Insert")
           if(all(whichAreInserts)) return(TRUE)
           return(paste("ERROR in ListOfInserts: ", which(!whichAreInserts)))
         })


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
v_ageVariable
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
                           gitAction="push",
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

