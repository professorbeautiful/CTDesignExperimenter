objectToStringList = function(obj) {
  name = as.character(substitute(obj))
  if(is.null(obj))
    return(NULL)
  if(is.list(obj)) {
    #return(obj)
    answer = lapply(obj, objectToStringList)
    if(is.null(names(obj))){
      for(i in 1:length(answer)) 
        answer[[i]] = c( 
                        children=list(answer[[i]]))
    } else {
      for(i in 1:length(answer)) 
        answer[[i]] = c(name=names(obj)[i], 
                        children=list(answer[[i]]))
    }
    names(answer) = NULL   
    return(answer)
  }
  if(is.character(obj)) {
    if(length(obj)==1)
      return(obj)
    else return(list(name=name, children=as.list(obj)))
  }
  if(is.numeric(obj)) {
    obj = as.character(obj)
    if(length(obj)==1)
      return(obj)
    else return(list(name=name, children=as.list(obj)))
  }
  if(is.function(obj)) {
    fString = functionToString(obj)
    if(length(grep("<environment", fString[length(fString)])) > 0)
      fString = fString[-length(fString)]
    fString = paste(fString, collapse="\n")
    return(fString)
    #return(list(name=name, children=fString))
  }
  if(is.object(obj)) {
    slotContents = sapply(simplify=FALSE,
                          X=names(getSlots(class(obj))), 
                          slot, object=obj)
    dataPart = try(obj@.Data, silent=TRUE)
    if(class(dataPart) != "try-error") 
      slotContents = c(dataPart=dataPart, slotContents)
    #answer = lapply(slotContents, objectToStringList)
    answer = objectToStringList(slotContents)
    for(i in 1:length(answer)) 
      answer[[i]] = c(name=names(slotContents)[[i]], 
                      children=list(answer[[i]]))
    names(answer) = NULL
    return(list(name=objname, children=list(answer)))
  }
  browser(text="objectToStringList: unhandled object type ")
}

objectToJSON = function(obj) {
  call = sys.call()
  call[[1]] = "objectToStringList"
  print(call)
  invisible(eval(call))
  #cat(toJSON())
}

objectToJSON(v_ageCategoryVariable)

# require("RJSONIO")
# 
# #objectToStringList(list(letters[1:3], 1:4))
# objectToStringList(v_ageCategoryVariable)
# cat(toJSON(..(), pretty=T))
# # objectToStringList(vg_clearanceRate)
# # objectToStringList(defaultScenario)
# #scenarioList = objectToStringList(defaultScenario)
# 
# 
# #scenarioJSON = toJSON(scenarioList, pretty=TRUE)
# #scenarioJSON = gsub("\t", "  ", scenarioJSON)
# ##pbcopy(scenarioJSON)
# #theFile = file("/Users/Roger/Sites/d3playground/defaultScenario.json")
# #writeLines(scenarioJSON, theFile)
# #close(theFile)
# 
# #toJSON(list(a=1:3, b=(4:8)))
# 
# objectToStringList(list(a=list(C=8,D=9), b=(4:8)))
# toJSON(list(a=list(C=8,D=9), b=(4:8)))
# toJSON(..())
# 
# list(name="a", 
#      children=list(name="C", children=list(name="CC", name="8"), 
#                    c(name="D")))
# list(name="a", 
#      children=c(list(name="C"), 
#                    list(children=list(name="D"))))
# cat(rjson::toJSON(..()))
# cat(RJSONIO::toJSON(..(), pretty=T))
# 
# list(name="v_ageCategoryVariable", 
#                 children=list(c(name="name", 
#                                 list(children=list(name="dose"))), 
#                               c(name="description", 
#                                 list(children=list(name="dose"))),
#                               c(name="checkDataType", 
#                                 list(children=list(name="function (x)  .Primitive(\"is.numeric\")")))
#                 )
# )
# scenarioJSON = toJSON(..(), pretty=T)
# cat(scenarioJSON)
# #pbcopy(toJSON(..(), pretty=T))
# #theFile = file("/Users/Roger/Sites/d3playground/defaultScenario.json")
# theFile = file("ctde/inst/shinyD3//defaultScenario.json")
# writeLines(scenarioJSON, theFile)
# close(theFile)
#     
# 
# theFile = file("/Users/Roger/Sites/d3playground/flare.json")
# testJson = readLines(con=theFile)
# close(theFile)
# testJsonInR = fromJSON(testJson)
