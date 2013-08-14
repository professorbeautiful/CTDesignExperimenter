setClass("Variable", contains="character",
         slots=list(description="character", 
                                       dataType="character",
                                       dataTypeDetail="ANY"),
         prototype=prototype(description="This is my string variable", 
                             dataType="character")
)

sexAsCharacter = new("Variable", "sex", description="my sex variable", dataType="character")

setClass("VariableValue", contains="character",
         representation=representation(variable="Variable", value="ANY"),
         prototype=prototype(variable=sexAsCharacter, value="Male"),
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

writeVariableFile = function(name, description, dataType, dataTypeFacet="", 
                             author=system("echo $USER",intern=TRUE),
                             time=Sys.time()){
  filename = paste(name, as.numeric(time), "R", sep=".")
  code = "new('Variable', " %&% name %&%
    ', description=' %&% description %&%
    ', dataType=' %&% dataType %&%
    ', dataTypeFacet=' %&% dataTypeFacet %&%
    ")"
  print(code)
}

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
# getVariableValues = function(generator){
#   if(is.null(generator@requirements))
#     return(list(evaluateOutput(generator)))
#   input = sapply(generator@requirements, getVariableValues)
#   output = evaluateOutput(generator, input)
#   return(c(input, output))
#     ### Combine the generator's inputs with its output variable.
# }


# setMethod("generatePatientFeatures",
#           signature(variableGenerator="SimpleVariableGenerator"),
#           function(variableGenerator){
#             evaluateOutput(variableGenerator)
#           }
# )

# list2env: Since environments are never duplicated, the argument envir is also changed.
# GOOD get("A", env=as.environment(list(A=10)))
# GOOD eval(expression(A), env=as.environment(list(A=10)))

extractRequirements = function(generator){
#  formalArgs(generator@generatorCode)
  generator@requirements
}


#extractRequirements(clearanceRate)

# extractProvisions = function(generator){
#   generator@outputName
# }

# extractProvisions(clearanceRate)
#  NOT SURE ABOUT THIS.
# For a VariableGenerator,
# the name of the generator is the provision.
# For a VariableGenerator, or PopulationModel,
# it's the c() of its VariableGenerators.




setClass("VariableNetwork", contains="Specifier")
###  for combining variables into a patient description model.
### -- or call it "VariableGeneratorBundle" ???
# Then PopulationModel can be a subclass of VariableNetwork.



## This will allow you to access a variable's value like this:
sex = "F"
get(new("Variable", "sex", description="my sex", dataType="factor"))
### "sex" will NOT be the value of this; "F" will.
###  So its "contains" can be a value!  Not what's intended though.
### Potentially very confusing.

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

