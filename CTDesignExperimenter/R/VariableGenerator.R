cat("======== VariableGenerator.R ================\n")

setClass("VariableGenerator", contains="Specifier",
         slots=list(
           insertSubType="character",
           outputVariable="Variable"
           ### this will be the provision. At first, a string. Later, a Variable.
           , generatorCode="function" 
           ### Arguments are the requirements,
           ### which are VariableValues.
         )
         ,
         validity=function(object) { # has to be "object"
           if( ! (object@insertSubType %in% scaffoldInsertSubTypes))
             return(paste0("Invalid insertSubType: ", object@insertSubType))
           return(TRUE)
           }
)

VariableGenerator = function(insertSubType="PatientAttribute",
                             name="NEED NAME",
                             parameters=list(),
                             provisions, 
                             requirements=NULL,
                             outputVariable, generatorCode) {
  if(missing(provisions)) provisions=outputVariable
  if(missing(outputVariable)) outputVariable=provisions
  vg = new("VariableGenerator", 
           name=name,
           timestamp=Sys.time(),
           author=Sys.getenv("USER"),
           insertSubType=insertSubType,
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

setMethod(f="show", signature=signature("VariableGenerator"),
          definition=function(object){
            cat("VG: output: ")
            show(object@outputVariable)
            cat("VG: requirements: ")
            if(length(object@requirements) == 0) cat(" None\n")
            else cat(paste(sapply(object@requirements, show), "\n"))    
            cat("VG: code: ")
            cat(printFunctionBody(object@generatorCode))
            cat("\n------------\n")
          })

makeVariableGeneratorConstructors <- function () {
  for(insertSubType in scaffoldInsertSubTypes) {
    if(insertSubType != "") {
      text = (paste0("assign(insertSubType, function(...)
          VariableGenerator(insertSubType='", insertSubType,
                   "', ...), pos=1)"))
      eval(parse(text=text))
    }
  }
}

setMethod("print", "VariableGenerator", function(x){
  cat("   VG: output: ", capture.output(x@provisions), "\n")
  for (req in x@requirements) {
    cat("       req: ", req, "\n") ### Omits all but the Variable name slot.
  }
}
)          


