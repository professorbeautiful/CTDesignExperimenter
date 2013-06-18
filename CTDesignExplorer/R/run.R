run = function(background=FALSE, createCTDEcatalog=TRUE) {
  if(createCTDEcatalog) createCTDEcatalog()
#   if(background) { ## Run it in the background outside of this session.
#     system("echo 'library(CTDesignExplorer); run()' | R --vanilla")
  #   PROBLEMS!  Warning in .simpleDuplicateClass(def, prev) :
#  the specification for S3 class “AsIs” in package ‘BiocGenerics’ seems equivalent to one from package ‘RJSONIO’ and is not turning on duplicate class definitions for this class
#   }
  result = tryCatch( error = function(e) e,
    expr=require("shiny"))
  if(class(result) != "try-error")
    runApp(system.file(
      package="CTDesignExplorer", "shinydocs"))
  ### NOTE that the folder inst/shinydocs 
  ### becomes just shinydocs after building.
  return(invisible(NULL))
}