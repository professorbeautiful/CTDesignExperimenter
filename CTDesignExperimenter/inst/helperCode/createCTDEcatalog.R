cat("======== createCTDEcatalog.R ================\n")

createVariableCatalog = function() {
  v_sexVariable = new("Variable", name="sex", 
                      description="my sex variable, as an unrestricted string", dataType="character")  
  v_ageVariable = new("Variable", name="age", 
                      description="age, as an unrestricted number", dataType="numeric")
  v_ageCategoryVariable = new("Variable", name="age", 
                              description="age, binned by decade", 
                              dataType="factor",
                              dataTypeDetail=paste0("(", 10*(0:9), ",", 10*(1:10), "]"))
  v_clearanceRate = new("Variable", name="clearanceRate", description="generic clearance rate variable",
                        dataType="numeric")
  v_toxDoseThreshold = new("Variable", name="toxDoseThreshold", description="dose threshold for binary toxicity event",
                           dataType="numeric")
  v_responseDoseThreshold = new("Variable", name="responseDoseThreshold", description="dose threshold for binary response event",
                                dataType="numeric")  
}

### Tests:
# new("VariableValue", 94, variable=v_ageVariable)
# try(silent=TRUE,
#     new("VariableValue", "xyz", variable=v_ageVariable)) ### fail

createVariableGeneratorCatalog = function() {
  
}  

createCTDEcatalog = function() {
  ### Based on rogerTestsYuanyuanCode.R
  #print(ls())  ### Initially, nothing.
  local(envir=.GlobalEnv, expr={ 
    
    threePlusThree_five_tiers = 
      new("APlusBSpecifier", TierDoses=1:5)
    crm9 <- new("CRMSpecifier",
                InitialProbGuesses=c(0.1,0.2,0.3,0.4,0.5),
                TargetProb=0.3,
                SampleSize=21,
                NPatsPerCohort=3,
                StartingDoseLevel=3,
                TierDoses=1:5,
                InitialStageDoseLevels=NULL)
    # PopModelSpecs = list(NULL)  ## incorrect for signature.
    setClass(Class="PKclearanceModel",
             contains="BaseCharModelSpecifier",
             representation=representation(
               location="numeric", sd="numeric"),
             prototype=prototype(
               location=15, sd=0.5,
               RGenFun="exp(rnorm(1,mean=log(baseCharModelSpec@location),
             sd=baseCharModelSpec@sd))")
    )
    standardPKclearanceModel = new("PKclearanceModel",
                                   BaseCharName = "PKclearance") 
    
    setClass(Class="ToxDoseThresholdModel",
             contains="BaseCharModelSpecifier",
             representation=representation(
               location="numeric", sd="numeric"),
             prototype=prototype(
               ConditionBaseCharNames = "PKclearance",
               location=1, sd=0.02,
               RGenFun="PKclearance * exp(rnorm(baseCharModelSpec@location, sd=baseCharModelSpec@sd))")
    )
    
    standardToxDoseThresholdModel = new("ToxDoseThresholdModel",
                                        BaseCharName = "ToxDoseThreshold") 
    
    setClass(Class="EfficacyDoseThresholdModel",
             contains="BaseCharModelSpecifier",
             representation=representation(
               location="numeric", sd="numeric"),
             prototype=prototype(
               ConditionBaseCharNames = "PKclearance",
               location=1, sd=0.02,
               RGenFun="PKclearance * exp(rnorm(baseCharModelSpec@location, sd=baseCharModelSpec@sd))")
    )
    standardEfficacyDoseThresholdModel = new("EfficacyDoseThresholdModel",
                                             BaseCharName = "efficacyDoseThreshold") 
    
    doseThresholdPopModelSpec = new("PopModelSpecifier")  ## which is a list.
    doseThresholdPopModelSpec@PopModelSpec[[1]] = standardPKclearanceModel
    doseThresholdPopModelSpec@PopModelSpec[[2]] = standardToxDoseThresholdModel
    doseThresholdPopModelSpec@PopModelSpec[[3]] = standardEfficacyDoseThresholdModel
    # lapply(1:3, function(x)doseThresholdPopModelSpec
    #                   @PopModelSpec[[x]]@location)
    
    toxDoseThresholdOutcomeModel <- new("DoseThresholdModelSpecifier",
                                        DoseThresholdName="ToxDoseThreshold")
    efficacyDoseThresholdOutcomeModel <- new("DoseThresholdModelSpecifier",
                                             DoseThresholdName="EfficacyDoseThreshold")
    p2bd = new("Phase2BryantDaySpecifier", 
               N1Pats=15, NPats=30, Efficacy1LL=2, EfficacyLL=8,
               NonTox1LL=2, NonToxLL=8)
    ########  
    
    evalPctPatsAtEachDose = new("EvalPctPatsAtEachDoseSpecifier")
    
    evalNToxsAtEachDose = new("EvalNToxsAtEachDoseSpecifier")
    
    ### currently the validation allows only these two DoseThresholdName.
    ### Now, to export the objects to .GlobalEnv!
    print(ls())
    ### the following line works
    #   temp=function() { xtemp=111; 
    #                     assign("xtemp", get("xtemp"), pos=1)}
    #   temp()
    ## but this doesn't.  Ahh, now it does. Why?
  })
  #   for(obj in ls()) assign(obj, get(obj), pos=1)
  #   return(ls())
}
