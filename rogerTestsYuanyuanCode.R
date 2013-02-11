library("CTDesignExplorerDevel")
crm9 <- new("CRMSpecifier",
	InitialProbGuesses=c(0.1,0.2,0.3,0.4,0.5),
	TargetProb=0.3,
	SampleSize=21,
	NPatsPerCohort=3,
	StartingDoseLevel=3,
	TierDoses=1:5,
	InitialStageDoseLevels=NULL)

designSpecs <- list(crm9)

# PopModelSpecs = list(NULL)  ## incorrect for signature.


doseThresholdPopModelSpec = new("PopModelSpecifier")  ## which is a list.
doseThresholdPopModelSpec@PopModelSpec[[1]] = 
  new("BaseCharModelSpecifier",
      BaseCharName = "PKclearance",
      RGenFun = "exp(rnorm(1,mean=log(15),sd=0.5))") 
doseThresholdPopModelSpec@PopModelSpec[[2]] = 
  new("BaseCharModelSpecifier",
      BaseCharName = "ToxDoseThreshold",
      ConditionBaseCharNames = "PKclearance",
      RGenFun = "PKclearance * exp(rnorm(1, sd=0.02))") 
doseThresholdPopModelSpec@PopModelSpec[[3]] = 
  new("BaseCharModelSpecifier",
      BaseCharName = "ResponseDoseThreshold",
      ConditionBaseCharNames = "PKclearance",
      RGenFun = "PKclearance * exp(rnorm(1, sd=0.02))") 

ToxDoseThresholdModelSpec <- new("DoseThresholdModelSpecifier",
				DoseThresholdName="ToxDoseThreshold")
OutcomeModelSpecs <- list(ToxDoseThresholdModelSpec)

EvalSpec1 <- new("EvalSampleSizeSpecifier")
EvalSpec2 <- new("EvalNToxsSpecifier")
EvalSpec3 <- new("EvalRP2DSpecifier")
EvalSpecs <- list(EvalSpec1,EvalSpec2,EvalSpec3)

oneCTresult = sim1CT(designSpec=crm9,
		popModelSpec=doseThresholdPopModelSpec,   ## correct.
		outcomeModelSpec=ToxDoseThresholdModelSpec)
oneCTresult

anExperiment = doExperiment(
	designSpecs, popModelSpecs=list(doseThresholdPopModelSpec), 
	outcomeModelSpecs=list(ToxDoseThresholdModelSpec), 
	evalSpecs=EvalSpecs, 
	nReps=2, 
	seed=NULL, simDataDir="./", userInput=FALSE) 
length(anExperiment)  #  1;  just one scenario.
length(anExperiment$Scenario1)  #  3 = length(EvalSpecs)
anExperiment$Scenario1[[1]] #sample sizes, per rep;  summary => table
anExperiment$Scenario1[[2]] # toxicities per rep;  summary => mean and variance
anExperiment$Scenario1[[3]] # RP2D, per re;  summary => mean, variance, NAs
