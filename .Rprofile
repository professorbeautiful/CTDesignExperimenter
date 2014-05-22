if(! interactive()){
	cat("Skipping .Rprofile...\n")
}
if(interactive()){
### In RStudio,  .Rprofile does work. .First does not.
#sink("/dev/null")
cat("Running .Rprofile from the CTDE project.\n")
#theStartingTime = date()
#cat(theStartingTime)
#cat("Before loading the package, the search path is:\n")
#print(search())
##[1] ".GlobalEnv"      "package:methods" "Autoloads"       "package:base"  
library("mvbutils")
attach("~/Dropbox/_HOME/R-in-Dropbox/R-utilities/.RData")
library("utils")
##library("CTDesignExperimenter")
## error: could not find function "packageDescription" because utils was not attached.
## But, do we really want to attach the package?

	options(repos="http://cran.r-project.org")
	options(texi2dvi="pdflatex")
	options(stringsAsFactors = FALSE)

	if(Sys.getenv("R_USER") ==  "")
	  Sys.setenv(R_USER=Sys.getenv("HOME"))

	options(program.editor = function(name, fname) paste("open ", 
	    fname, "", sep = ""), edit.scratchdir = "/Users/Roger/R.scratch", 
	    backup.fix = c(2L, 2L))
	autoedit( TRUE)
	options( mlazy.index=TRUE)
	options(program.editor = function(name, fname)
	  paste("open ", fname, "", sep = ""),
		edit.scratchdir = "~/R.scratch", backup.fix = 2:3)
	options(menu.graphics = TRUE)
	options(write.mvb.tasks = TRUE)
	#NO LONGER NEEDED!  assign.to.base("help", base.help, override.env = FALSE)
	Sys.setenv(DISPLAY = ":0")
	abbreviate.cdprompt = 3
	Sys.setlocale("LC_COLLATE", "en_US.UTF-8")

options(devtools.install.args="--no-multiarch")

#
## createCTDEcatalog()
## run()
##  Error in get(obName) : invalid first argument
## But the traceback says:
## 5: Sys.sleep(0.001)
## 4: tryCatchList(expr, classes, parentenv, handlers)
## 3: tryCatch(while (TRUE) {
##   serviceApp()
##   Sys.sleep(0.001)
## }, finally = {
##   timerCallbacks$clear()
## })
## 2: runApp(system.file(package = "CTDesignExplorer", "shinydocs"))
## 1: run()
### The LHS of the page loads.
#
## library("mstoolkit") ## Last active Jly 3 2011 on R-forge
##  archived by CRAN.
#print(search())
#
#sink()
}
