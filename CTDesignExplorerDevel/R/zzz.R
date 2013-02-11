
.onAttach = function(libname, pkgname) {
	desc <- packageDescription(pkgname)
	packageStartupMessage("This is ", pkgname, " ", desc$Version, " ", desc$Date)
	return(invisible(NULL))
}
