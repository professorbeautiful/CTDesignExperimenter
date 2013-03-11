library("CTDesignExplorer")
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
### currently the validation allows only these two DoseThresholdName.

showMethods("sim1CT", includeDefs=T)
oneCTresult = sim1CT(designSpec=crm9,
		popModelSpec=doseThresholdPopModelSpec,   ## correct.
    #### only one outcome allowed in sim1CT call.
    outcomeModelSpec=toxDoseThresholdOutcomeModel)
oneCTresult



#  [1] "PatsData"    "CTTimes"     "Conclusions"
#  "ID"  "BaseChars"  "ConcurrentTrtsDataList" "PatTimes" 
oneCTresult@PatsData[[4]]@ID #NULL
oneCTresult@PatsData[[4]]@BaseChars
# PKclearance      ToxDoseThreshold efficacyDoseThreshold 
# 10.83429              10.71533              10.64177 
oneCTresult@PatsData[[4]]@ConcurrentTrtsDataList
### includes TrtAllos, Outcomes, and TimesToOutcomes
oneCTresult@PatsData[[4]]@PatTimes  ### NULL

###########  Now, prepare an experiment

evalSpec1 <- new("EvalSampleSizeSpecifier")
evalSpec2 <- new("EvalNToxsSpecifier")
evalSpec3 <- new("EvalRP2DSpecifier")
evalSpecs <- list(evalSpec1,evalSpec2,evalSpec3)
outcomeModelSpecs <- list(toxDoseThresholdOutcomeModel,
                          efficacyDoseThresholdOutcomeModel )

anExperiment = doExperiment(
  designSpec=designSpecs,
  popModelSpecs=list(doseThresholdPopModelSpec), 
	outcomeModelSpecs=list(toxDoseThresholdOutcomeModel, efficacyDoseThresholdOutcomeModel), 
	evalSpecs=evalSpecs, 
	nReps=2, 
	seed=NULL, simDataDir="./", userInput=FALSE) 
length(anExperiment)  #  1;  just one scenario.
length(anExperiment$Scenario1)  #  3 = length(EvalSpecs)
anExperiment$Scenario1[[1]] #sample sizes, per rep;  summary => table
anExperiment$Scenario1[[2]] # toxicities per rep;  summary => mean and variance
anExperiment$Scenario1[[3]] # RP2D, per re;  summary => mean, variance, NAs
