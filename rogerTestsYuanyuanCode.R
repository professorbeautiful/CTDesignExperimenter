crm9 <- new("CRMSpecifier",
	InitialProbGuesses=c(0.1,0.2,0.3,0.4,0.5),
	TargetProb=0.3,
	SampleSize=21,
	NPatsPerCohort=3,
	StartingDoseLevel=3,
	TierDoses=1:5,
	InitialStageDoseLevels=NULL)
DesignSpecs <- list(crm9)

PopModelSpecs = list(NULL)

ToxDoseThresholdModelSpec <- new("DoseThresholdModelSpecifier",
				DoseThresholdName="ToxDoseThreshold")
OutcomeModelSpecs <- list(ToxDoseThresholdModelSpec)

EvalSpec1 <- new("EvalSampleSizeSpecifier")
EvalSpec2 <- new("EvalNToxsSpecifier")
EvalSpec3 <- new("EvalRP2DSpecifier")
EvalSpecs <- list(EvalSpec1,EvalSpec2,EvalSpec3)

oneCTresult = sim1CT(designSpec=crm9,
		popModelSpec=PopModelSpec,
#		popModelSpec=NULL,
		outcomeModelSpec=ToxDoseThresholdModelSpec)
oneCTresult
anExperiment = doExperiment(
	designSpecs, popModelSpecs=list(PopModelSpec), 
	outcomeModelSpecs=list(ToxDoseThresholdModelSpec), 
	evalSpecs=EvalSpecs, 
	nReps=200, 
	seed=NULL, simDataDir="./") 
length(anExperiment$Scenario1)  #  3 = length(EvalSpecs)