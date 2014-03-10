
.onAttach = function(libname, pkgname) {
	desc <- packageDescription(pkgname)
	packageStartupMessage("This is ", pkgname, " ", desc$Version, " ", desc$Date)
	#	createCTDEcatalog()    # Can't write into locked env.
	makeScaffoldObjects()
	makeVariableGeneratorConstructors()
	vc = createVariableCatalog()
	for(v in ls(env=vc)) 
    assign(v, value=get(v, env=vc), pos=1)
	makeDefaultScenario()
	return(invisible(NULL))
}
