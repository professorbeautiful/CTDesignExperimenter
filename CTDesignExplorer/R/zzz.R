
.onAttach = function(libname, pkgname) {
	desc <- packageDescription(pkgname)
	packageStartupMessage("This is ", pkgname, " ", desc$Version, " ", desc$Date)
	#	createCTDEcatalog()    # Can't write into locked env.
  cat("Run createCTDEcatalog() to get started.\n")
	return(invisible(NULL))
}
