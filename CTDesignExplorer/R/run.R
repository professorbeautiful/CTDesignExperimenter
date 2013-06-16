run = function() {
  createCTDEcatalog()
  result = tryCatch( error = function(e) e,
    expr=require("shiny"))
  if(class(result) != "try-error")
    runApp(system.file(
      package="CTDesignExplorer", "shinydocs"))
  ### NOTE that the folder inst/shinydocs 
  ### becomes just shinydocs after building.
  return(invisible(NULL))
}