
makeDefaultScenario = function() {
  
    vg_age = PatientAttribute(parameters=list(ageMean=50, ageCV=0.5),
                              generatorCode=function(){
                                rlognorm(1, ageMean, ageCV)
                              },
                              provisions=v_ageVariable
    )
    
    ec_age = EligibilityCriterion( parameters=list(cutoff=50),
                                   requirements=VariableList(v_ageVariable),
                                   outputVariable=Variable(name="isOldEnough",
                                                           description="Patient is old enough",
                                                           checkDataType=is.logical),
                                   generatorCode=function()  { age >= cutoff}
    )  #### OK.

    v_liverVariable = Variable(name="liverFunction", desc="Liver function", 
                               gitAction="none",
                               checkDataType=function(x) is.numeric(x))
    vg_liver = PatientAttribute(parameters=list(liverMean=1, liverCV=0.2),
                                generatorCode=function(){
                                  rlognorm(1, liverMean, liverCV)
                                },
                                provisions=v_liverVariable
    )
    ec_liver = EligibilityCriterion( parameters=list(cutoff=1.5),
                                     requirements=VariableList(v_liverVariable),
                                     outputVariable=Variable(name="liverOK",
                                                             description="Patient has sufficient liver function.",
                                                             checkDataType=is.logical),
                                     generatorCode=function()  { liverFunction <= cutoff}
    )  #### OK.
    
    #  TODO 
    # st_oneDose = as(Class="ScheduleTreatment",
    #                 EventGenerator(parameters=list(theDose),
    #                                generatorCode=))
  
  vg_clearanceRate = VariableGenerator(
    parameters=list(clearanceLocation=6,
                    clearanceSD=1),
    provisions=v_clearanceRate,
    requirements=VariableList(list()),
    #outputName="clearanceRate", 
    ### Maybe we don't need outputName? Use the object name, right?
    generatorCode=function() {
      #We don't really need Multiplier. Just testing out the parameter idea.
      #      if(missing(Multiplier)) Multiplier = 1
      return(exp(rnorm(1, mean=log(clearanceLocation), sd=clearanceSD))
      )  
    }
  )
  
  
  evaluateVNoutputs(VariableNetwork(vgList=VariableGeneratorList(vg_clearanceRate)))
  ######################
  
  vg_responseDoseThreshold  = VariableGenerator(
    parameters=list(responseLoc=0, responseSD=0.01),
    requirements=VariableList(v_clearanceRate),
    provisions=v_responseDoseThreshold, 
    generatorCode=function() { 
      clearanceRate * 
        exp(rnorm(1, responseLoc, responseSD))
    }
  )
  
  
  #evaluateOutput(vg_responseDoseThreshold)
  
  vg_toxDoseThreshold  = VariableGenerator(
    parameters=list(toxLoc=0.5, toxSD=0.1),
    requirements=VariableList(v_clearanceRate),
    provisions=v_toxDoseThreshold, 
    generatorCode=function() { 
      clearanceRate * 
        exp(rnorm(1, toxLoc, toxSD))
    }
  )
  
  # evaluateOutput(vg_toxDoseThreshold)
  
  vNpharm = VariableNetwork(vgList=VariableGeneratorList(
    vgList=list(
      vg_clearanceRate=vg_clearanceRate, 
      vg_responseDoseThreshold=vg_responseDoseThreshold, 
      vg_toxDoseThreshold=vg_toxDoseThreshold))
  )  #### you need the names?
  
  tempEnv = evaluateVNoutputs(vNpharm)
  
  #printNetworkOutputs(tempEnv)
  #######
  
  v_SampleSizeMax = Variable(name="SampleSizeMax", 
                             description='Upper bound for sample size', 
                             checkDataType=is.numeric)
  
  createVG_FixedSampleSizeMax = function(Nmax = 3) {
    VariableGenerator(insertSubType="DesignParameter", 
                      parameters=list(SampleSizeMax=Nmax),
                      provisions=v_SampleSizeMax, 
                      generatorCode=function(){
                        SampleSizeMax
                      }
    )
  }
  
  # TESTING scenarioNoElig = getVGs(defaultScenario, "PatientAttribute")
  
  v_dose = Variable(name="dose", description="dose", checkDataType=is.numeric)
  vg_dose = VariableGenerator(insertSubType="ScheduleTreatment", 
                              parameters=list(dose=5),
                              provisions=v_dose,
                              generatorCode=function()dose
  )
                              
  v_responseOutcome = Variable(name="response", description="Clinical response (binary)",
                               checkDataType=function(x){
                                 is.logical(x) & length(x)==1
                               }
  )

  vg_responseOutcome = VariableGenerator(insertSubType="PatientOutcome", 
                                         provisions=v_responseOutcome, 
                                         requirements=VariableList(list(v_dose,v_responseDoseThreshold)),
                                         generatorCode=function()
                                           responseDoseThreshold < dose
                                         )
  
  v_SampleSizeMaxIsReached = Variable(name="SampleSizeMaxIsReached", 
                                      description='Upper bound for sample size has been reached.', 
                                      checkDataType=is.logical)
  vg_SampleSizeMaxIsReached = VariableGenerator(
    insertSubType="StoppingCriterion", 
    requirements=VariableList(v_SampleSizeMax),
    provisions=v_SampleSizeMaxIsReached, 
    generatorCode=function(){
      SampleSizeMax <= trialData$trialSummaries$NpatientsEnrolled
    }
  )
  ## Creating a default scenario #####
  
  defaultScenario =  #as("Scenario",
    new("ListOfInserts", 
       list(
      vg_liver=vg_liver, vg_age=vg_age, 
      ec_liver=ec_liver, ec_age=ec_age,
      vg_clearanceRate=vg_clearanceRate, 
      vg_responseDoseThreshold=vg_responseDoseThreshold,
      vg_toxDoseThreshold=vg_toxDoseThreshold,
      vg_dose=vg_dose,
      vg_responseOutcome=vg_responseOutcome,
      vg_SampleSizeMax = createVG_FixedSampleSizeMax(2),
      vg_SampleSizeMaxIsReached = vg_SampleSizeMaxIsReached)
      #,name="defaultScenario", 
      #description="default scenario"
    )# )
  for(obj in ls()) {
    tryresult = try(assign(obj, get(obj), pos=1))
    if(class(tryresult) == "try-error")
      cat("try-error: makeDefaultScenario: obj is ", obj, "\n")
  }
  #length(getVGs(scenario=defaultScenario, subType="PatientAttribute"))
  #length(getVGs(scenario=defaultScenario, subType="EligibilityCriterion"))
  
}

# setMethod("print", "Scenario", definition = function(scen) )