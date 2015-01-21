
.onAttach = function(libname, pkgname) {
	desc <- packageDescription(pkgname)
	packageStartupMessage("This is ", pkgname, " ", desc$Version, " ", desc$Date)
	#	createCTDEcatalog()    # Can't write into locked env.
  #startup()
	loadLatestScenario()
	return(invisible(NULL))
}

startup = function(useExistingDefaults=TRUE, useLastScenario=TRUE){
  makeScaffoldObjects()
  if ( useLastScenario) {
    result = try( loadLatestScenario())
    if(class(result) != "try-error")
      assign("defaultScenario", currentScenario, pos=1)
    else cat("Error in startup(): ", defaultScenario, "\n")
  }
  if( ( exists("defaultScenario", where=1) & useExistingDefaults) )
    return(invisible())
  ### no defaultScenario, or we want to revert to the "stock" scenario
  makeVariableGeneratorConstructors()
  vc = createVariableCatalog()
  for(v in ls(env=vc)) 
    assign(v, value=get(v, env=vc), pos=1)
  makeDefaultScenario()
}
