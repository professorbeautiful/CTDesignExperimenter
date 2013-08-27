
#' generatePatientsForAccrual
# Assume generating patient baseline charatceristics is the first action that occurs on a patient 
# nPats: number of patients
generatePatientsForAccrual = function(popModelSpec,nPats,currentCTData){ 
        if (!missing(popModelSpec))
           NPatsData <- sapply(1:nPats,function(x) 
             new("PatData",BaseChars=generateBaseChars(popModelSpec)))  
        else 
          NPatsData <- sapply(1:nPats,function(x) new("PatData"))
        currentCTData@PatsData <- c(currentCTData@PatsData,NPatsData)  
        return (list(NewCTData=currentCTData))
    }
)# It returns a list with one element for NewCTData
    




########################################################################################################
#                                          The Master Code for Doing Experiment
#                                           S4 Method: doExperiment
########################################################################################################

## Method: sim1CT
# This method simulates a single CT data under the specified population model (optional, which can be NULL), design, outcome model.            
setGeneric("sim1CT",function(designSpec,popModelSpec,outcomeModelSpec) standardGeneric("sim1CT"))
setMethod("sim1CT",signature(designSpec="DesignSpecifier",
                             popModelSpec="OptionalPopModelSpecifier",
                             outcomeModelSpec="OutcomeModelSpecifier"),
    function(designSpec,popModelSpec,outcomeModelSpec){
        CurrentCTData <- new("CTData")
        CurrentActionQ <- new("ActionQueue",ActionQ=generateInitialActions(designSpec))
        while(length(CurrentActionQ@ActionQ) != 0){
            CurrentAction <- CurrentActionQ@ActionQ[[1]] 
            getOtherArgs(CurrentAction)
            CurrentGlobalTime <- CurrentAction@GlobalTime
            Output <- eval(parse(text=CurrentAction@MethodCall))
            if (!is.null(Output$NewCTData)) CurrentCTData <- Output$NewCTData
            if (!is.null(Output$NewActions)) {
                for ( i in 1:length(Output$NewActions)) 
                    CurrentActionQ <- addAction(currentActionQ=CurrentActionQ,newAction=Output$NewActions[[i]])
            }
            CurrentActionQ@ActionQ <- CurrentActionQ@ActionQ[-1]
        }
    return(CurrentCTData)
    }
)

## Method: checkRequirements

# This method is to check requirements among designs, population models,outcome models and evaluation criteria
setGeneric("checkRequirementsNew",
           function(needy, giver, ...) standardGeneric("checkRequirementsNew"))

setGeneric("reportInteroperability",
           function(needy, giver, ...) standardGeneric("reportInteroperability"))

setMethod("checkRequirementsNew", 
          signature("Specifier", "Specifier"),  ### cannot include ... here
          function(needy, giver){  
            #            subx = (substitute(needy))     
            #             if (is.name(subx)) 
            #               subx <- deparse(subx)
            #             print(subx) ### From fix()... but it doesn't work here.
            req = getRequirements(needy)
            prov = getProvisions(giver)
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
        

             
