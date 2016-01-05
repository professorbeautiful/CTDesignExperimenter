cat("======== run  ================\n")

#' run
#' 
#' Run shiny app form CTDesignExperimenter:
#' shinyInterface:  the whole trip (the default)
#' shinyScenario: a detailed viewer of a single scenario
#' 
#' @param background Run in the background-- not yet working.
#' @param shinyfolder Default is "shinyInterface", alternative is "shinyScenario".
#' @param local If FALSE, run version in the package; otherwise relative to current folder.
#' @param createCTDEcatalog Not used.

run = function(background=FALSE, shinyfolder="shinyInterface", local=FALSE, createCTDEcatalog=FALSE) {
#   if(createCTDEcatalog) createCTDEcatalog()
  #   if(background) { ## Run it in the background outside of this session.
  #     system("echo 'library(CTDesignExplorer); run()' | R --vanilla")
  #   PROBLEMS!  Warning in .simpleDuplicateClass(def, prev) :
  #  the specification for S3 class “AsIs” in package ‘BiocGenerics’ seems equivalent to one from package ‘RJSONIO’ and is not turning on duplicate class definitions for this class
  #   }
  result = tryCatch( error = function(e) e,
                     expr=require("shiny"))
  if(class(result) != "try-error"){
    if(local) {
      cat("Running locally\n")
      runApp(paste0("CTDesignExperimenter/inst/", shinyfolder))
    }
    else {
      cat("Running from package folder\n")
      runApp(system.file(
        package="CTDesignExperimenter", shinyfolder))
    }
    ### NOTE that the folder inst/shinydocs 
    ### becomes just shinydocs after building.
  }
  return(invisible(NULL))
}