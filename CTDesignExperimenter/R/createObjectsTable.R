createObjectsTable = function(theSpecChoice) {
  if(missing(theSpecChoice))
    theSpecChoice = switch((input$viewChoice),
                           `View spec objects`=(input$specChoiceModels),
                           `Define one clinical trial`= (input$specChoiceOneCT)
    )    #### TODO-- handle NULL.""trying to get slot \"className\" from an object of a basic class (\"NULL\") with no slots","
  if(is.null(theSpecChoice)) theSpecChoice = "PopModelSpecifier"
  if(regexpr("\\[", theSpecChoice) > 0)  ### Remove extra characters
    theSpecChoice = substring(theSpecChoice, 1, regexpr("\\[", theSpecChoice) - 2)
  theObjects = data.frame(stringsAsFactors=FALSE,
                          instanceNames(theSpecChoice))
  names(theObjects) = "instance"
  print(theObjects)
  theObjects = theObjects[order(theObjects$instance), , drop=FALSE]
  theObjects$class = sapply(theObjects$instance,
                            FUN=function(obName) class(get(obName)))
  theObjects$requirements = 
    sapply(theObjects$instance, 
           FUN=function(theObject){
             req = try(getRequirements(get(theObject)), silent=TRUE)
             if(class(req) == "try-error") return ("")
             return(paste(req, collapse="\n"))}
    )
  theObjects$provisions = 
    sapply(theObjects$instance, 
           FUN=function(theObject){
             prov = try(getProvisions(get(theObject)), silent=TRUE)
             if(class(prov) == "try-error") return ("")
             return(paste(prov, collapse="\n"))}
    )
  #    cat("\n==theObjects==\n")
  #    print(str(theObjects))
  theObjects
}

instanceNames = function(className) {
  names(which(sapply(.GlobalEnv, is, className )))
}
#debug(instanceNames)
