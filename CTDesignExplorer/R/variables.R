cat("======== Variables.R ================\n")

setClass("Variable", 
         slots=list(name="character", description="character", 
                                       checkDataType="function"),
         prototype=prototype(name="variableName", 
                             description="This is my  variable's description", 
                             checkDataType=function()TRUE))
Variable = function(name="variableName", 
                    description="This is my string variable", 
                    checkDataType=function()TRUE){
                      if(!is.function(checkDataType))
                        stop("Variable: checkDataType should be a function.")
                      return(new("Variable", name=name,
                                 description = description,
                                 checkDataType=checkDataType))
                    }
setClass("VariableList", contains="list",
         validity=function(object){
           for(v in object){
#             print(v)
             if(!is(v, "Variable") & !is.null(v))
               return("VariableList is invalid: not a Variable")
           }
           return(TRUE)
         }
)
VariableList = function(vList) {
  if(!is(vList, "list")) vList = list(vList)
  new("VariableList", vList)
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

printFunctionBody = function(f) attributes(attributes(f)$srcref)$srcfile$lines

setMethod("print", "Variable", function(x)
  cat(" ", x@name, " (", printFunctionBody(x@checkDataType), ")\n")  ### Omits description.
)   

is.nonnegative.vector = function(x) { is.numeric(x) & all(x>=0)}
is.nonnegative.number = function(x) { is.numeric(x) & (x>=0)}

v_sexAsCharacter = new("Variable", name="sex", description="my sex variable", 
                       checkDataType=function(x) is(x,"character"))
print(v_sexAsCharacter)

setClass("VariableValue", contains="ANY",
         slots=list(variable="Variable"))
#,         validity=validityVariableValue)


VariableValue = function(value, variable) {
  dataPart = value
  ifVerboseCat("dataPart", dataPart)
  # ifVerboseCat("variable@checkDataType", variable@checkDataType)
  ifVerboseCat("typeof(dataPart)", typeof(dataPart))
  #  if( ! is(dataPart, variable@checkDataType)) ### why is this different??
  if( ! ( variable@checkDataType(dataPart)))
    stop(paste0("Invalid value for variable ", 
               variable@name,
               ", found ", typeof(dataPart),
         "but failed checkDataType():\n\t", 
         printFunctionBody( variable@checkDataType)
))
  return(new("VariableValue", dataPart, variable=variable))
}

#VariableValue(123, vA)

#VariableValue("xyz", vA)



# validityVariableValue = function(object){
##   confused about checkDataType somehow.
# #           print(object)  ### "Data part is undefined for general S4 object" !!!
#            dataPart = object@.Data
#            ifVerboseCat("dataPart", dataPart)
#            ifVerboseCat("object@variable@checkDataType", object@variable@checkDataType)
#            if(is(dataPart, object@variable@checkDataType))
#              return(TRUE)
#            return(paste("Invalid value for variable ", 
#                         object@variable@name,
#                         ". Looking for ", object@variable@checkDataType,
#                         ", found ", typeof(dataPart)))
#          }
# )
# new("VariableValue")

#writeVariableFile = function(name, description, checkDataType, checkDataTypeDetail="", 
writeVariableFile = function(variable, 
                             author=system("echo $USER",intern=TRUE),
                             time=Sys.time()){
  name = variable@name
  description = variable@description
  checkDataType = variable@checkDataType
  filename = paste(name, as.numeric(time), "R", sep=".")
  code = "new('Variable',  name='"  %&% 
    name %&%
    "', description='" %&% 
    description %&%
    "', checkDataType='" %&% 
    checkDataType %&%
    "'" %&% {if(!(checkDataTypeDetail=="")) 
      ", checkDataTypeDetail='" %&% checkDataTypeDetail %&% "'"}  %&%
    ")"
  write(code, filename)
  print(code)
}

###  writeVariableFile(name="howSmart", checkDataType="numeric", description="This is the guy-s IQ.")
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



setMethod("print", "VariableValue",
          function(x)
            cat("Variable: ", x@variable@.Data, "  Value: ", x@value, "\n"))

setMethod("names", "VariableList", function(x) sapply(x, function(v)v@name))