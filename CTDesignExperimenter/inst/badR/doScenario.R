cat("======== doScenario.R  ================\n")

setClass("Scenario", contains="VariableNetwork",
         slots=list(popModel="PopulationModel", design="Design", 
                    outcomeModel="OutcomeModel"))

####  Loop over sim1CT, collecting information to feed into the criteria ####



## Method: checkRequirements

# This method is to check requirements among designs, population models,outcome models and evaluation criteria
setGeneric("checkRequirementsNew",
           function(needed, given, ...) standardGeneric("checkRequirementsNew"))

setGeneric("reportInteroperability",
           function(needed, given, ...) standardGeneric("reportInteroperability"))

setMethod("checkRequirementsNew", 
          signature("Specifier", "Specifier"),  ### cannot include ... here
          function(needed, given){  
            #            subx = (substitute(needed))     
            #             if (is.name(subx)) 
            #               subx <- deparse(subx)
            #             print(subx) ### From fix()... but it doesn't work here.
            req = getRequirements(needed)
            prov = getProvisions(given)
            if(is.list(req))
              sapply(req, function(r) {
                print(r)
              })
            return(NULL)
          }
)

## Method: doExperiment
# nReps: number of clinical trial replications to simulate
# simDataDir: directory for saving nReps simulated CT data, for example, 
# "C:/Yuan/Research/RogerDay/Yuan/MyThesis/ExpPlatform/Data/", the name of simulated CT data is in the format of
# "SimCTsData_S1.RData", where S1 refers to scenario 1
# If userInput=TRUE, then the program will ask for permission to proceed after printing out the results from the
# requirements-checking.
setGeneric("doExperiment",function(designSpecs,popModelSpecs,outcomeModelSpecs,evalSpecs,nReps,seed,simDataDir,userInput) 
  standardGeneric("doExperiment"))

# If no population model is specified, then popModelSpecs=list(NULL)
setMethod("doExperiment",signature(designSpecs="list",popModelSpecs="list",outcomeModelSpecs="list",evalSpecs="list",
                                   nReps="numeric",seed="OptionalNumeric",simDataDir="OptionalCharacter",userInput="logical"),
          function(designSpecs,popModelSpecs,outcomeModelSpecs,evalSpecs,nReps,seed,simDataDir,userInput){
            if(!is.null(seed)) set.seed(seed)
            # Check requirements
            ValidSetsMatrix <- checkRequirements(designSpecs=designSpecs,popModelSpecs=popModelSpecs,outcomeModelSpecs=outcomeModelSpecs,
                                                 evalSpecs=evalSpecs)
            if (is.null(ValidSetsMatrix)) stop("No sets of designs, population models, outcome models and evaluation criteria can
                                               work together!")
            else{
              # Check user to see if he/she wants to proceed
              NValidSets <- nrow(ValidSetsMatrix)
              # Initialize "Scenarios", each scenario corresponds to a combination of a design, a population model and an 
              # outcome model
              Scenarios <- rep(1,NValidSets)
              for ( i in 2:NValidSets){
                if(any(ValidSetsMatrix[i-1,1:3]!=ValidSetsMatrix[i,1:3])) Scenarios[i] <- Scenarios[i-1]+1
                else Scenarios[i] <- Scenarios[i-1]
              }
              cat("Scenarios:","\n")
              print(cbind(Scenarios,ValidSetsMatrix))
              answer <- "Yes"
              if(userInput) answer <- readline("Proceed? (Yes or No)")
              if(answer=="No") print("Experiment is cancelled!")
              else if (answer=="Yes"){
                # Initialize the outputs from evaluation
                NScenarios <- length(unique(Scenarios))
                EvalOutputs <- vector("list",NScenarios)
                names(EvalOutputs) <- sapply(1:NScenarios,function(x) paste("Scenario",x,sep=""))
                for(ScenarioIndex in 1:NScenarios){
                  DesignSpec <- designSpecs[[(ValidSetsMatrix[Scenarios==ScenarioIndex,"Design"][1])]]
                  PopModelSpec <- popModelSpecs[[(ValidSetsMatrix[Scenarios==ScenarioIndex,"PopM"][1])]]
                  OutcomeModelSpec <- outcomeModelSpecs[[(ValidSetsMatrix[Scenarios==ScenarioIndex,"OutcomeM"][1])]]
                  EvalSpecs <- evalSpecs[(ValidSetsMatrix[Scenarios==ScenarioIndex,"Criterion"])]
                  SimCTsData <- sapply(1:nReps,function(x) sim1CT(designSpec=DesignSpec,popModelSpec=PopModelSpec,
                                                                  outcomeModelSpec=OutcomeModelSpec))
                  if(!is.null(simDataDir)) save(SimCTsData,file=paste(simDataDir,"SimCTsData_S",ScenarioIndex,".RData",sep=""))
                  EvalOutputs[[ScenarioIndex]] <- lapply(EvalSpecs,function(x) evalDesign(evalSpec=x,simCTsData=SimCTsData))
                }
                return(EvalOutputs)
              }
              else stop("Your input is wrong! It should be either Yes or No.")
            }
          }
)



