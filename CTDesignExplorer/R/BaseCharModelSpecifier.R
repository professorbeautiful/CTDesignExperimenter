### Classes and Methods for population model
## Class: BaseCharModelSpecifier
setClass("BaseCharModelSpecifier",
         contains="Specifier",
         representation(BaseCharName="character",
                        ConditionBaseCharNames="OptionalCharacter",
                        RGenFun="character"),
         prototype = list(BaseCharName="noName", 
                          ConditionBaseCharNames = NULL,
                          RGenFun="NA")
)
setMethod("initialize", signature=signature("BaseCharModelSpecifier"),
          function(.Object, ...) {
            .Object=callNextMethod(.Object, ...)
            .Object@parameters$BaseCharName = .Object@BaseCharName
            #            .Object@requirements = c(.Object@requirements, .Object@ConditionBaseCharNames)
            #            .Object@provisions = c(.Object@provisions, .Object@BaseCharName)  
            .Object
          })