setGeneric("objToDataframe", package="CTDesignExperimenter",
           def=function(x) {
             data.frame( 
               description=x@description,
               author=x@author,
               timestamp=capture.output(x@timestamp),
               filename=x@filename 
             )
           }
)
setMethod("objToDataframe", signature = "Variable",
          function(x) convertVariableToDataframe(x))
setMethod("objToDataframe", signature = "Insert",
          function(x) convertInsertToDataframe(x))
setMethod("objToDataframe", signature = "ListOfInserts",
          function(x) convertListOfInsertsToDataframe(x))
# objToDataframe(v_ageCategoryVariable)
# objToDataframe(vg_age)

convertVariableToDataframe <<- function(theVar){ ## assigned globally
  data.frame(
    name=theVar@name,
    description=theVar@description,
    checkDataType = printFunctionBody(theVar@checkDataType),
    author=theVar@author,
    timestamp=capture.output(theVar@timestamp),
    filename=theVar@filename      
  )
}

makeTemplateInsert = function() {
  vg = VariableGenerator(outputVariable = makeTemplateVariable(), generatorCode=function()TRUE)
  vg@name = "I_Template"
  vg@description = "(description here)"
  vg
}

makeTemplateVariable = function() 
  Variable(name = "(name)", description = "(description)", checkDataType = function(x)is.boolean(x))

convertInsertToDataframe = function(theInsert) {
  data.frame( 
    output=capture.output(print(theInsert@outputVariable)),
    parameters=ifelse(is.null(theInsert@parameters) | length(theInsert@parameters)==0,
                      character(0),
                      paste(names(theInsert@parameters), 
                            as.vector(capture.output(theInsert@parameters)),
                            sep="=", collapse="\n")
    ),
    needed=ifelse(is.null(theInsert@requirements),
                  "",
                  paste(sapply(theInsert@requirements, capture.output), 
                        collapse="\n")
    ),
    generator=printFunctionBody(theInsert@generatorCode),
    author=theInsert@author,
    timestamp=capture.output(theInsert@timestamp),
    filename=theInsert@filename          
  )
}


convertListOfInsertsToDataframe = function(x) {
  data.frame( 
    description=x@description,
    author=x@author,
    timestamp=capture.output(x@timestamp),
    filename=x@filename 
  )
}

objToDataframe(currentScenario)


