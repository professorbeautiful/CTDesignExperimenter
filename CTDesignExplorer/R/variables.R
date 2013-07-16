setClass("Variable", contains="character",
         representation=representation(description="character", 
                                       dataType="character",
                                       dataTypeDetail="ANY"),
         prototype=prototype(description="This is my string variable", 
                             dataType="character")
)
## This will allow you to access a variable's value like this:
##   sex = "M"
###  get(new("Variable", "sex", description="my sex", dataType="factor"))

### Later we can change dataType to a function that validates a value.

sexVariable = new("Variable", "sex", 
                  description="my sex variable, as an unrestricted string", dataType="character")
### simpler than subclassing, I think.
ageVariable = new("Variable", "age", 
                  description="age, as an unrestricted number", dataType="numeric")
ageCategoryVariable = new("Variable", "age", 
                  description="age, binned by decade", 
                          dataType="factor",
                          dataTypeDetail=paste0("(", 10*(0:9), ",", 10*(1:10), "]"))
clearanceRateVariable = new("Variable", "clearanceRateVariable", description="generic clearance rate variable",
                            dataType="numeric")
############################

setClass("VariableValue", contains="character",
         representation=representation(variable="Variable", value="ANY"),
         prototype=prototype(variable=sexVariable, value="Male"),
         validity=function(object){
           if(is(object@value, object@variable@dataType))
             return(TRUE)
           #We should also check,if it's a factor, that the value is one of the
           #levels of that factor.
           return(paste("Invalid value for variable ", object@variable,
                        ". Looking for ", object@variable@dataType,
                        ", found ", typeof(object@value)))
         }
)
# new("VariableValue")
ageValue = new("VariableValue", variable=ageVariable, value=94)
clearanceRateValue = new("VariableValue", variable=clearanceRateVariable, value=1)@variable@.Data

############################################################
#' @title   A component of a PopulationModel, generating just one variable value.
#' 
#' \code{SimpleVariableGenerator} S4 class for a component of a PopulationModel, 
#' generating just one variable value
#' , and adding it to the values of the inputs.
#'  \section{Slots} {
#'   \describe{
#'   \item{\code{outputVariable}:}{An object of class \code{Variable}}
#'   \item{\code{generatorCode}:}{A function calculating the value for the variable.}
#'   }}
setClass("VariableGenerator", contains="Specifier",
         representation=representation(
           outputVariable="Variable"
            ### this will be the provision. At first, a string. Later, a Variable.
           , generatorCode="function" 
           ### Arguments are the requirements,
           ### which are VariableValues.
         ),
         validity=function(object) { # has to be "object"
           if(length(object@requirements)==0) 
             return(TRUE)## For now.
           else {
             for(req in object@requirements){
               if(!is("VariableGenerator", req))
                 return(paste0("VariableGenerator: error: ",
                               "req not a VariableGenerator:\n",
                               req))
             }
             return(TRUE)
           }
         }
)
#setClass("DependentVariableGenerator", contains="SimpleVariableGenerator",
  ###  Take each "requirement", which is a VariableGenerator,
         # and recursively generates its value.
         # Then
}
### We should not need separate DependentVariableGenerator and SimpleVariableGenerator
getVariableValues = function(generator){
  if(is.null(generator@requirements))
    return(list(evaluateOutput(generator)))
  input = sapply(generator@requirements, getVariableValues)
  output = evaluateOutput(generator, input)
  return(c(input, output))
    ### Combine the generator's inputs with its output variable.
}
         
clearanceRate = new("VariableGenerator",
                    parameters=list(location=15, sdev=0.5),
                    provisions=list("clearanceRate"),
                    outputVariable=clearanceRateVariable,
                    #outputName="clearanceRate", 
                    ### Maybe we don't need outputName? Use the object name, right?
    generatorCode=function() {
      #We don't really need Multiplier. Just testing out the parameter idea.
#      if(missing(Multiplier)) Multiplier = 1
      return(exp(rnorm(1,mean=log(location), sd=sdev))
      )
    }
)

#' Evaluate a generator expression.
#' 
#' \code{evaluateOutput} 
#' \param generator A SimpleVariableGenerator 
#' \param input Input "requirements" list of VariableValue objects.
evaluateOutput = function(generator, input) {
  ## Make the generatorCode function available.
  if(!is(generator, "VariableGenerator"))
    stop("evaluateOutput: generator should be a VariableGenerator")
  ## Make the generator itself visible.
  inputs = as.environment(list(theGeneratorCode=generator@generatorCode))
  ## Make the parameters available.
  inputs = list2env(generator@parameters, inputs)
  if(!missing(input)){
    ## Make the inputs (requirements) available.
    if(!is.list(input)) { ### input is a single variable
      input = list(input)
      if(length(extractRequirements(generator))==1)
        names(input) = extractRequirements(generator)
      else
        stop("evaluateOutput: input is not a list, but the requirements length is not 1.")
    }
    inputs = list2env(input, inputs)
    command = paste0("theGeneratorCode(",
                     paste(names(input), collapse=", "),
                     ")")
  }
  else
    command = "theGeneratorCode()"
  #  print(sys.call())
  catn("command:", command)
  print(ls(env=inputs))
  catn("inputs: \n")
  print(inputs)
  catn("is.list(inputs)", is.list(inputs))
#  environment(inputs$theGeneratorCode) = inputs
  environment(generator@generatorCode) = 
    list2env(generator@parameters, environment(generator@generatorCode))
  value = generator@generatorCode()
#  value = eval(parse(text="inputs$theGeneratorCode()"))
  print(value)
  print(generator@outputVariable)
  return(new("VariableValue", variable=generator@outputVariable, value=value))
}

evaluateOutput(clearanceRate)


# setMethod("generatePatientFeatures",
#           signature(variableGenerator="SimpleVariableGenerator"),
#           function(variableGenerator){
#             evaluateOutput(variableGenerator)
#           }
# )
setMethod("generateBaseChar",
          signature(baseCharModelSpec="BaseCharModelSpecifier"),
          function(baseCharModelSpec){
evaluateOutput(clearanceRate,0)
evaluateOutput(clearanceRate) ## OK works if no input provided.

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
                        parameters=list(location=15, sdev=0.5),
                        requirements=list(),
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
