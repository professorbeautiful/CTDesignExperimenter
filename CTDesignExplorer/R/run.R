run = function() {
  result = tryCatch( error = function(e) e,
    expr=require("shiny"))
  if(class(result) != "try-error")
    runApp(system.file(
      package="CTDesignExplorer", "inst", "shinydocs"))
  return(invisible(NULL))
}