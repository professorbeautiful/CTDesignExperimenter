cat("======== Variables.R ================\n")

setClass("Variable", 
         slots=list(name="character", description="character", 
                                       dataType="character",
                                       dataTypeDetail="ANY"),
         prototype=prototype(name="variableName", 
                             description="This is my string variable", 
                             dataType="character",dataTypeDetail=NULLo
)

setClass("VariableList", contains="list",
         validity=function(object){
           for(v in object){
#             print(v)
             if(!is(v, "Variable"))
               return("VariableList is invalid: not a Variable")
           }
           return(TRUE)
         }
)
VariableList = function(vList) {
  if(!is(vList, "list")) vList = list(vList)
}
#' Variables
#' 
#' Allows VariableGenerators to use either one Variable or a list of them in provisions and requirements.
#' 
setClassUnion("Variables", c("Variable", "VariableList", "NULL"))

#' Specifier
#' 
#' Superclass of VariableGenerator, PopulationModel, Design, OutcomeModel, Criterion
setClass("Specifier", 
  #       contains="character", ### maybe a name will be desired.
         slots=list(
           parameters="list", 
           requirements="Variables",
           provisions="Variables"),
         prototype=list(parameters=list(), 
                        requirements=NULL,
                        provisions=NULL)
)

setMethod("print", "Variable", function(x)
  cat(" ", x@name, " (", x@dataType, ")\n")  ### Omits description.
)   

v_sexAsCharacter = new("Variable", name="sex", description="my sex variable", dataType="character")
print(v_sexAsCharacter)

setClass("VariableValue", contains="ANY",
         slots=list(variable="Variable"),
         validity=function(object){
#           print(object)  ### "Data part is undefined for general S4 object" !!!
           dataPart = object@.Data
           print(is(dataPart, "numeric"))
           if(is(dataPart, object@variable@dataType))
             return(TRUE)
           return(paste("Invalid value for variable ", 
                        object@variable@name,
                        ". Looking for ", object@variable@dataType,
                        ", found ", typeof(dataPart)))
         }
)
# new("VariableValue")

#### I don't know if we will use this...
writeVariableFile = function(name, description, dataType, dataTypeDetail="", 
                             author=system("echo $USER",intern=TRUE),
                             time=Sys.time()){
  filename = paste(name, as.numeric(time), "R", sep=".")
  code = "new('Variable',  name='"  %&% 
    name %&%
    "', description='" %&% 
    description %&%
    "', dataType='" %&% 
    dataType %&%
    "'" %&% {if(!(dataTypeDetail=="")) 
      ", dataTypeDetail='" %&% dataTypeDetail %&% "'"}  %&%
    ")"
  write(code, filename)
  print(code)
}

writeVariableFile(name="howSmart", dataType="numeric", description="This is the guy-s IQ.")
### You can't use dump on S4 objects.

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

v_sexVariable = new("Variable", name="sex", 
                  description="my sex variable, as an unrestricted string", dataType="character")
### simpler than subclassing, I think.

v_ageVariable = new("Variable", name="age", 
                  description="age, as an unrestricted number", dataType="numeric")
v_ageCategoryVariable = new("Variable", name="age", 
                          description="age, binned by decade", 
                          dataType="factor",
                          dataTypeDetail=paste0("(", 10*(0:9), ",", 10*(1:10), "]"))
v_clearanceRate = new("Variable", name="clearanceRate", description="generic clearance rate variable",
                            dataType="numeric")
v_toxDoseThreshold = new("Variable", name="toxDoseThreshold", description="dose threshold for binary toxicity event",
                            dataType="numeric")
v_responseDoseThreshold = new("Variable", name="responseDoseThreshold", description="dose threshold for binary response event",
                       dataType="numeric")

new("VariableValue", 94, variable=v_ageVariable)
try(silent=TRUE,
    new("VariableValue", "xyz", variable=v_ageVariable)) ### fail


setMethod("print", "VariableValue",
          function(x)
            cat("Variable: ", x@variable@.Data, "  Value: ", x@value, "\n"))

