
setGeneric("allRequirements", function(network, ...){})
setMethod("allRequirements", signature="VariableGenerator",
          function(network){
            c(network@requirements,
              ifelse(length(network@requirements) > 0,
                     lapply(network@requirements, extractRequirements),
                     NULL))
          })
setMethod("allRequirements", signature="PopModel",
          function(network){
            c(
            lapply(network@requirements,
                   `@`, "provisions" )
            ,
            lapply(network@requirements, allRequirements)
            )
          })
