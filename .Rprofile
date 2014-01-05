### In RStudio,  .Rprofile does work. .First does not.
#sink("/dev/null")
#cat("Running .Rprofile from the CTDE project.\n")
#theStartingTime = date()
#cat(theStartingTime)
#cat("Before loading the package, the search path is:\n")
#print(search())
##[1] ".GlobalEnv"      "package:methods" "Autoloads"       "package:base"  
#attach("~/Dropbox/_HOME/R-in-Dropbox/R-utilities/.Rdata")
#library("utils")
##library("CTDesignExplorer")
## error: could not find function "packageDescription" because utils was not attached.
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
