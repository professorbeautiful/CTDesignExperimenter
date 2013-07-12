setClass("Variable", contains="character",
         representation=representation(description="character", dataType="character"),
         prototype=prototype(description="This is my string variable", dataType="character")
)
## This will allow you to access a variable's value like this:
##   sex = "M"
###  get(new("Variable", "sex", description="my sex", dataType="factor"))

### Later we can change dataType to a function that validates a value.

sexClass = new("Variable", "sex", description="my sex variable", dataType="character")
### simpler than subclassing, I think.

setClass("VariableValue", contains="character",
         representation=representation(variable="Variable", value="ANY"),
         prototype=prototype(variable=sexClass, value="Male"),
         validity=function(object){
           if(is(object@value, object@variable@dataType))
             return(TRUE)
           return(paste("Invalid value for variable ", object@variable,
                        ". Looking for ", object@variable@dataType,
                        ", found ", typeof(object@value)))
         }
)
# new("VariableValue")
#  new("VariableValue", variable=ageVariable, value=94)

setClass("SimpleVariableGenerator", contains="Specifier",
         representation=representation(
           outputName="character" ### this will be the provision. At first, a string. Later, a Variable.
           , generatorCode="function" ### arguments are the requirements.
         )
)


clearanceRate = new("SimpleVariableGenerator",
                    parameters=list(location=15, sdev=0.5),
                    outputName="myOutput", 
                    ### But outputName might as well be the object name, right?
    generatorCode=function(input) {
      Multiplier = input$Multiplier
      exp(rnorm(1,mean=log(location), sd=sdev*Multiplier))
                    }
)

evaluateOutput = function(generator, input) {
  ## Make the generatorCode function available.
  params = as.environment(list(generatorCode=generator@generatorCode))
  ## Make the parameters available.
  params = list2env(generator@parameters, params)
  ## Make the inputs (requirements) available.
  params = list2env(list(input=input), params)
  # print(ls(env=params))
  eval(expression(generatorCode(input)),
       envir=params)
}

evaluateOutput(clearanceRate, list(Multiplier=0))

# list2env: Since environments are never duplicated, the argument envir is also changed.
# GOOD get("A", env=as.environment(list(A=10)))
# GOOD eval(expression(A), env=as.environment(list(A=10)))

extractRequirements = function(generator){
  formalArgs(generator@generatorCode)
}
extractRequirements(clearanceRate)

extractProvisions = function(generator){
  generator@outputName
}
extractProvisions(clearanceRate)

##### ok to here.########
toxDoseThreshold  = new("SimpleVariableGenerator",
                        #parameters=list(location=15, sdev=0.5),
                        outputName="toxDoseThreshold", 
                        ### But outputName might as well be the object name, right?
                        generatorCode=function(B) { ## B just to test conditional.
                          PKclearance * 
                            exp(rnorm(baseCharModelSpec@location,
                                      sd=baseCharModelSpec@sd)))
)))
                        }
)


setClass(Class="ToxDoseThresholdModel",
         contains="BaseCharModelSpecifier",
         representation=representation(
           location="numeric", sd="numeric"),
         prototype=prototype(
           ConditionBaseCharNames = "PKclearance",
           location=1, sd=0.02,
           RGenFun="PKclearance * exp(rnorm(baseCharModelSpec@location, sd=baseCharModelSpec@sd))")
)

standardToxDoseThresholdModel = new("ToxDoseThresholdModel",
                                    BaseCharName = "ToxDoseThreshold") 



setClass("VariableNetwork", contains="Specifier")
###  for combining variables into a patient description model.
