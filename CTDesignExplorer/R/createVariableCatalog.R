createVariableCatalog = function() {
  v_sexVariable = new("Variable", name="sex", 
                      description="my sex variable, as an unrestricted string", dataType="character")  
  v_ageVariable = new("Variable", name="age", 
                      description="age, as an unrestricted number", dataType="double")
  v_ageCategoryVariable = new("Variable", name="age", 
                              description="age, binned by decade", 
                              dataType="factor",
                              dataTypeDetail=paste0("(", 10*(0:9), ",", 10*(1:10), "]"))
  v_clearanceRate = new("Variable", name="clearanceRate", description="generic clearance rate variable",
                        dataType="double")
  v_toxDoseThreshold = new("Variable", name="toxDoseThreshold", description="dose threshold for binary toxicity event",
                           dataType="double")
  v_responseDoseThreshold = new("Variable", name="responseDoseThreshold", description="dose threshold for binary response event",
                                dataType="double")  
  for(v in ls()) assign(v, get(v), pos=1)
}

#createVariableCatalog()