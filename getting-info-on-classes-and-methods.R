### getting info on classes and methods

getClass("BetaSpecifier")
getSlots("BetaSpecifier")
slotNames("BetaSpecifier")

showMethods(f="getMean")
showMethods(f="getMean", classes="BetaSpecifier")
showMethods(f="getMean", classes="BetaSpecifier", includeDefs=F)
methods( class="BetaSpecifier")  ###None????
length(showMethods(classes="BetaSpecifier", printTo=F))  # > 1400


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

