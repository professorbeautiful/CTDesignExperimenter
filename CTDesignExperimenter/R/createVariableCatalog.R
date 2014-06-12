createVariableCatalog = function() {
  v_sexVariable = Variable( name="sex", 
                      description="my sex variable, as an unrestricted string", checkDataType=is.character)  
  v_ageVariable = Variable( name="age", 
                      description="age, as a nonnegative number", 
                      checkDataType=function(x)is.double(x)&(x>=0))
  v_ageCategoryVariable = Variable( name="age", 
                              description="age, binned by decade", 
                              checkDataType=function(x)is.factor(x) & 
                              levels(x) %in% paste0("(", 10*(0:9), ",", 10*(1:10), "]"))
  v_clearanceRate = Variable( name="clearanceRate", description="generic clearance rate variable",
                        checkDataType=function(x)is.double(x)&(x>=0))
  v_toxDoseThreshold = Variable( name="toxDoseThreshold", description="dose threshold for binary toxicity event",
                           checkDataType=function(x)is.double(x)&(x>=0))
  v_responseDoseThreshold = Variable( name="responseDoseThreshold", description="dose threshold for binary response event",
                                checkDataType=function(x)is.double(x)&(x>=0))
  v_SampleSizeMax = Variable(name="SampleSizeMax", 
                             description='Upper bound for sample size', 
                             checkDataType=is.numeric)
  return(sys.frame(sys.nframe()))
}


