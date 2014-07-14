### NOT USED

output$varHoTable = renderHtable({
  theVar = findObjectInScenario(rValues$treeSelectionIndex)
  catn("output$varHoTable: var is ", capture.output(theVar))
  #sapply(names(getSlots("Variable")), slot, object=theVar)
  CHECK<-capture.output(theVar@checkDataType)
  result = data.frame(
    name=theVar@name,
    description=theVar@description,
    checkDataType = gsub("[\t ]+", " ", 
                         paste(collapse="", 
                               CHECK [-length(CHECK)] ) ) ,
    author=theVar@author,
    timestamp=as.cat(capture.output(theVar@timestamp)),
    filename=theVar@filename      
  )
  return(as.data.frame(result))
})
