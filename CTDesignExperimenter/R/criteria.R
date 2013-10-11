cat("======== criteria.R  ================\n")


## Method: evalDesign
setGeneric("evalDesign", function(evalSpec,simCTsData) standardGeneric("evalDesign"))

setMethod("evalDesign",signature(evalSpec="EvalSampleSizeSpecifier",simCTsData="list"),
          function(evalSpec,simCTsData){
            SampleSizes <- sapply(simCTsData,function(x) length(x@PatsData))
            Mean <- mean(SampleSizes)
            names(Mean) <- "Mean"
            Variance <- var(SampleSizes)
            names(Variance) <- "Variance"
            EvalOutput <- list(SampleSizes=SampleSizes,Summary=c(Mean,Variance))
            return(EvalOutput)
          }
)

# This method evaluates the toxicity outcomes from the first concurrent treatments
setMethod("evalDesign",signature(evalSpec="EvalNToxsSpecifier",simCTsData="list"),
          function(evalSpec,simCTsData){
            NToxs <- sapply(simCTsData,function(x) sum(sapply(x@PatsData, function(y) 
              y@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])))
            Mean <- mean(NToxs)
            names(Mean) <- "Mean"
            Variance <- var(NToxs)
            names(Variance) <- "Variance"
            EvalOutput <- list(NToxs=NToxs,Summary=c(Mean,Variance))
            return(EvalOutput)
          }
)

# This method evaluates the chosen RP2Ds from clinical trials
setMethod("evalDesign",signature(evalSpec="EvalRP2DSpecifier",simCTsData="list"),
          function(evalSpec,simCTsData){
            RP2Ds <- sapply(simCTsData,function(x) as.numeric(x@Conclusions["RP2D"]))
            Mean <- mean(RP2Ds,na.rm=TRUE)
            names(Mean) <- "Mean"
            Variance <- var(RP2Ds,na.rm=TRUE)
            names(Variance) <- "Variance"
            NNAs <- sum(is.na(RP2Ds))
            names(NNAs) <- "Number of NAs"
            EvalOutput <- list(RP2Ds=RP2Ds,Summary=c(Mean,Variance,NNAs))
            return(EvalOutput)
          }
)

# This method evaluates the probability of each tier dose being chosen as RP2D  
# during the first concurrent treatments in the trials.
# This method returns a named vector of the probabilities of each dose (including NA)being chosen as RP2D in the ascending order of
# tier doses.
setMethod("evalDesign",signature(evalSpec="EvalProbRP2DAtEachDoseSpecifier",simCTsData="list"),
          function(evalSpec,simCTsData){
            NReps <- length(simCTsData)
            # TierDosesC: vector of tier doses where each tier dose is coerced to be a character
            TierDosesC <- as.character(evalSpec@TierDoses)
            ProbsRP2D <- rep(0,length(TierDosesC))
            names(ProbsRP2D) <- TierDosesC
            RP2Ds <- sapply(simCTsData,function(x) as.numeric(x@Conclusions["RP2D"]))
            ProbRP2DTable <- round(table(RP2Ds)/NReps,3)
            ProbsRP2D[names(ProbRP2DTable)] <- ProbRP2DTable
            if(sum(is.na(RP2Ds)!=0))
              ProbsRP2D <- c(ProbsRP2D,"NA"=round(sum(is.na(RP2Ds))/NReps,3))
            return(list(ProbsRP2D=ProbsRP2D))
          }
)

# This method evaluates the number of patients allocated at each tier dose 
# during the first concurrent treatments in the trials.
# This method returns a list with a named vector of the average numbers of patients in the ascending order of
# tier doses and a matrix whose columns contain numbers of patients allocated for each dose
setMethod("evalDesign",signature(evalSpec="EvalNPatsAtEachDoseSpecifier",simCTsData="list"),
          function(evalSpec,simCTsData){
            NReps <- length(simCTsData)
            # TierDosesC: vector of tier doses where each tier dose is coerced to be a character
            TierDosesC <- as.character(evalSpec@TierDoses)
            # Number of patients at each tier dose = number of times each tier dose being assigned
            # AllNPatsMatrix stores the number of patients at each tier dose at each CT replication
            AllNPatsMatrix <- matrix(0,nrow=NReps,ncol=length(TierDosesC))
            colnames(AllNPatsMatrix) <- TierDosesC
            for (RepIndex in 1:NReps){
              AssignedDoses <- sapply(simCTsData[[RepIndex]]@PatsData,function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
              AssignedDoseTable <- table(AssignedDoses)
              AllNPatsMatrix[RepIndex,names(AssignedDoseTable)] <- AssignedDoseTable
            }
            NPats <- apply(AllNPatsMatrix,2,function(x) round(mean(x),3))
            return(list(AverageNPats=NPats,NPatsMatrix=AllNPatsMatrix))
          }
)

# This method evaluates the percentage of patients allocated at each tier dose 
# during the first concurrent treatments in the trials.
# This method returns a a named vector of the percentages of patients in the ascending order of
# tier doses.
setMethod("evalDesign",signature(evalSpec="EvalPctPatsAtEachDoseSpecifier",simCTsData="list"),
          function(evalSpec,simCTsData){
            NPats <- evalDesign(evalSpec="EvalNPatsAtEachDoseSpecifier",simCTsData=simCTsData)
            # For percentage of patients, which is calculated as Lin does in his "pmtd" program for validation purpose
            # Average total number of patients enrolled in a CT replication
            AverageNPats <- NPats$AverageNPats
            AverageTotNPats <- sum(AverageNPats)
            PctPats <- sapply(1:length(AverageNPats),function(x) round((AverageNPats[x]/AverageTotNPats)*100,3))
            names(PctPats) <- names(NPats)
            return(list(PctPats=PctPats))
          }
)

# This method evaluates the number of toxicities at each tier dose 
# during the first concurrent treatments in the trials.
# This method returns a list with a named vector of the average numbers of toxicities in the ascending order of
# tier doses and a matrix whose columns contain numbers of toxicities allocated for each dose
setMethod("evalDesign",signature(evalSpec="EvalNToxsAtEachDoseSpecifier",simCTsData="list"),
          function(evalSpec,simCTsData){
            NReps <- length(simCTsData)
            # TierDosesC: vector of tier doses where each tier dose is coerced to be a character
            TierDosesC <- as.character(evalSpec@TierDoses)
            # AllNToxsMatrix stores the number of toxicities at each tier dose at each CT replication
            AllNToxsMatrix <- matrix(0,nrow=NReps,ncol=length(TierDosesC))
            colnames(AllNToxsMatrix) <- TierDosesC
            for (RepIndex in 1:NReps){
              AssignedDoses <- sapply(simCTsData[[RepIndex]]@PatsData,function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
              Toxs <- sapply(simCTsData[[RepIndex]]@PatsData,function(x) x@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])
              if(any(Toxs==1)){
                ToxByAssignedDoseTable <- table(Toxs,AssignedDoses)["1",]
                AllNToxsMatrix[RepIndex,names(ToxByAssignedDoseTable)] <- ToxByAssignedDoseTable
              }
            }
            NToxs <- apply(AllNToxsMatrix,2,function(x) round(mean(x),3))
            return(list(AverageNToxs=NToxs,NToxsMatrix=AllNToxsMatrix))
          }
)

# This method calculates the toxicity rate as Lin does for exp. all tox. rate in his "pmtd" program
# for validation purpose
setMethod("evalDesign",signature(evalSpec="EvalToxRateSpecifier",simCTsData="list"),
          function(evalSpec,simCTsData){
            NToxs <- sapply(simCTsData,function(x) sum(sapply(x@PatsData, function(y) 
              y@ConcurrentTrtsDataList[[1]]@Outcomes["BinaryToxicity"])))
            SampleSizes <- sapply(simCTsData,function(x) length(x@PatsData))
            EvalOutput <- sum(NToxs)/sum(SampleSizes)
            names(EvalOutput) <- "ToxRate"
            return(EvalOutput)
          }
)
