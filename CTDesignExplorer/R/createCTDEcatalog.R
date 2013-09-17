cat("======== createCTDEcatalog.R ================\n")

createCTDEcatalog = function() {
  createVariableCatalog() 
  createVariableGeneratorCatalog() 
  createPopulationModelCatalog() 
  createDesignCatalog()
  createOutcomeModelCatalog() 
}

