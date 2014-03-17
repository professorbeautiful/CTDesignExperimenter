objectToStringList = function(obj) {
  if(is.null(obj))
    return(NULL)
  if(is.list(obj)) {
    return(lapply(obj, objectToStringList))
  }
  if(is.character(obj)) {
    if(length(obj)==1)
      return(obj)
    else return(as.list(obj))
  }
  if(is.numeric(obj)) {
    obj = as.character(obj)
    if(length(obj)==1)
      return(obj)
    else return(as.list(obj))
  }
  if(is.function(obj)) {
    fString = functionToString(obj)
    if(length(grep("<environment", fString[length(fString)])) > 0)
      fString = fString[-length(fString)]
    return(fString)
  }
  if(is.object(obj)) {
    slotContents = sapply(simplify=FALSE,
                          X=names(getSlots(class(obj))), slot, object=obj)
    dataPart = try(obj@.Data, silent=TRUE)
    if(class(dataPart) != "try-error") 
      slotContents = c(dataPart=dataPart, slotContents)
    answer = lapply(slotContents, objectToStringList)
    
    return(answer)
  }
  browser(text="objectToStringList: unhandled object type ")
}


#objectToStringList(list(letters[1:3], 1:4))
#objectToStringList(v_ageCategoryVariable)
#objectToStringList(vg_clearanceRate)
#objectToStringList(defaultScenario)
require("RJSONIO")
pbcopy(toJSON(objectToStringList(defaultScenario)))
