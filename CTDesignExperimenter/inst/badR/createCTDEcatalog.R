cat("======== createCTDEcatalog.R ================\n")

createCTDEcatalog = function() {
  createVariableCatalog() 
  createVariableGeneratorCatalog() 
  createPopulationModelCatalog() 
  #createDesignCatalog()  # no longer relevant
  createOutcomeModelCatalog() 
}


