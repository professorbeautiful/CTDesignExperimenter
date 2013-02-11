### getting info on classes and methods

getClass("BetaSpecifier")  ### displays the class itself!
getSlots("BetaSpecifier")
slotNames("BetaSpecifier")
findClass("BetaSpecifier") ## where in the search list?
find("BetaSpecifier") ## can't find it in the search list this way!

getMethod("getMean", signature="BetaSpecifier")
getMethod("getMean", signature="BetaSpecifier")

getClass(ToxDoseThresholdModelSpec) ## displays the class of this OBJECT! (DoseThresholdModelSpecifier) Returns the class.
getClass("DoseThresholdModelSpecifier")  ### 
getSlots(getClass("DoseThresholdModelSpecifier"))  ### just DoseThresholdName
getClassName  ### defunct!

temp = new(Class="BetaSpecifier", Shape1=10, Shape2=20)
temp@Shape1
getMean(temp)
getValidity(temp) # not defined.


showMethods(f="doExperiment")
showMethods(f="getMean")
showMethods(f="getMean", classes="BetaSpecifier")
showMethods(f="getMean", classes="BetaSpecifier", includeDefs=F)
showMethods(f="getMean", classes="BetaSpecifier", includeDefs=T)
showMethods(f="getMean", classes="ContinuousDistrSpecifier") ## none.
methods( class="BetaSpecifier")  ###None--   it find S3 methods.
length(showMethods(classes="BetaSpecifier", printTo=F))  # > 1400
table(0<regexpr("^Function", showMethods(classes="BetaSpecifier", printTo=F)),
      (1:length(showMethods(classes="BetaSpecifier", printTo=F))) %% 3)
      # Hmm.  No structure!  That's not very good.
## How can you reduce to the specific methods?  Here: 
showMethods(where="package:CTDesignExplorerDevel",
                   classes="BetaSpecifier", printTo=T)  # 6
grep(v=T, "^Function", showMethods(where="package:CTDesignExplorerDevel",
                  classes="BetaSpecifier", printTo=F))  
### Wow, is that the only way to get method names???

help(package="methods")  ###   S4
findMethods(f="getMean")
str(findMethods(f="getMean"))  ### class = "listOfMethods
findMethods(f="getMean")@names
findMethods(f="getMean")@signatures
slotNames("listOfMethods")
#[1] ".Data"      "arguments"  "signatures" "generic"    "names"  
findMethods(f="getMean")@.Data[[1]]  ### Aha!  getMean for BetaSpecifier

# Error in getMean(designSpec@BetaPriorSpec) : 
#   error in evaluating the argument 'distrSpec' in selecting a method 
# for function 'getMean': 
# Error: no slot of name "BetaPriorSpec" for this object of class "CRMSpecifier"


ls("package:CTDesignExplorer")
ls("package:CTDesignExplorerDevel")
# Find all available classes
getClasses(where="package:CTDesignExplorer")
getClasses(where="package:CTDesignExplorerDevel")
# Find documentation for the class ?CTData?
class?CTData
# Find documentation for the methods ?allocateTrts?
methods?allocateTrts

## suggested by john chambers,   https://stat.ethz.ch/pipermail/r-devel/2003-May/026640.html
trace("f", signature = "numeric", browser, exit = browser)

selectMethod(f, signature) # recommeded by Martin Morgan
showMethods("f", incl=TRUE) # recommeded by Martin Maechler
## See Doug Bates defense of S4, https://stat.ethz.ch/pipermail/r-devel/2008-January/047969.html
## But Hesterberg likes S3 better: https://stat.ethz.ch/pipermail/r-devel/2008-January/047967.html
# "In order to debug S4 methods (see Methods), you need to use trace, typically 
# calling browser, e.g., as "
# trace("plot", browser, exit=browser, signature = c("track", "missing"))

# "You can insert a call to browser() if you want to modify the source.  If
# you'd rather not do that, you can use trace() to set a breakpoint in it.
# The new setBreakpoint() function in R 2.10.0 will also work, if you
# install the package from source with the R_KEEP_PKG_SOURCE=yes
# environment variable set.  It allows you to set a breakpoint at a
# particular line number in the source code."

#I've opted for trace("cenreg", exit=recover), which has done the trick nicely. – Richie Cotton Nov 10 '09 at 15:38"
