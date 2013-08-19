cat("======== PopulationModel.R================\n")


setMethod("print", "PopulationModel",
          function(x) {
            x = as(object=x, Class="PopulationModel")
            cat("PopulationModel:\n ") 
            for(req in x@requirements) print(req)
            invisible(x)
          }
)
