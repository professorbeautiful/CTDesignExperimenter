##  Class: APlusBSpecifier
# "A+B with dose de-escalation" design was described in the article written by Lin in Biostatistics,v2,203-215,2001
# The default object is 3+3 design specification
setClass("APlusBSpecifier",
         representation(A="numeric",B="numeric",C="numeric",D="numeric",E="numeric", 
                        TierDoses="numeric"),
         contains="DesignSpecifier",
         ## Method: getRequirements
         # This method is to get requirements from an "APlusBSpecifier" object
         #          requirements=VariableList(
         #                      Outcomes="BinaryToxicity",TimesToOutcomes=character(0),BaseChars=character(0)))
         
         prototype=list(A=3,B=3,C=1,D=1,E=1,TierDoses=1:5),
          provisions=list(
            new("Variable", name="TrtAllos", 
                description="treatment assignments for all patients",
                dataType="numeric"),
            new("Variable", name="CTTimes", description="Times of important events in the life of the CT",
                dataType="numeric"),
            new("Variable", name="Conclusions", description="Recommended Phase2 dose",
                dataType="numeric"))
         ,
         validity=function(object){
           if(!all(Check<-c(object@B>0,object@C>0,object@C<=object@D,
                            object@D<=object@A,object@C<=object@E,object@E<=(object@D+object@B)))){
             Wrongs <- c("B<=0","C<=0","C>D","D>A","C>E","E>D+B")
             stop(paste(Wrongs[which(!Check)],collapse=","))
           }
           else TRUE
         })



# patsIndices: in the ascending order
setMethod("generateInitialActions",signature(designSpec="APlusBSpecifier"),
          function(designSpec){
            A <- designSpec@A
            # Generate A patients initially.
            Action1 <- new("Action",
                           MethodCall=
                             "generatePatientsForAccrual(
                           popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                           OtherArgs=list(NPats=A),GlobalTime=0)
            # Assign their treatments using allocateTrts()
            Action2 <- new("Action",MethodCall=
                             "allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,
                           patsIndices=PatsIndices)",
                           OtherArgs=list(PatsIndices=1:A),GlobalTime=1)
            InitialActions <- c(Action1,Action2)
            return(InitialActions)
          }
)# it returns a list of initial actions in the order of the times they are executed

# Method "checkStoppingRule" for the "A+B" design
setMethod("checkStoppingRule",
          signature(designSpec="APlusBSpecifier",
                    currentCTData="CTData",
                    currentGlobalTime="numeric"),
          function(designSpec,currentCTData,currentGlobalTime){
            # Design parameters
            A <- designSpec@A
            B <- designSpec@B
            C <- designSpec@C 
            D <- designSpec@D 
            E <- designSpec@E
            APlusB <- A+B
            NPats <- length(currentCTData@PatsData)
            TierDoses <- designSpec@TierDoses
            StartingDose <- TierDoses[1]
            HighestDose <- TierDoses[length(TierDoses)]
            CurrentDose <- currentCTData@PatsData[[NPats]]@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose
            CurrentDoseLevel <- which(TierDoses==CurrentDose)
            AppliedDoses <- sapply(currentCTData@PatsData[1:NPats],function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
            HighestAppliedDose <- max(AppliedDoses)
            # Indices for patients who are on current dose
            PatsIndicesOnCurrentDose <- (1:NPats)[AppliedDoses==CurrentDose]
            # Number of patients on current dose, which can be either A or (A+B)
            NPatsOnCurrentDose <- length(PatsIndicesOnCurrentDose)
            # Number of toxicity outcomes observed on current dose
            NToxsOnCurrentDose <- sum(rep(1,NPatsOnCurrentDose)[sapply(PatsIndicesOnCurrentDose,function(x)
              currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes == 1)])
            if ((NPatsOnCurrentDose==A & NToxsOnCurrentDose>D) 
                | (NPatsOnCurrentDose==APlusB & NToxsOnCurrentDose>E)){
              if (CurrentDose==StartingDose){
                currentCTData@Conclusions <- NA
                names(currentCTData@Conclusions) <- "RP2D"
                return (list(NewCTData=currentCTData))
              }
              else {
                # Dose that is one level lower than current dose
                Level1LowerDose <- TierDoses[CurrentDoseLevel-1]
                # Indices for patients who are on the dose with one level lower than current dose
                PatsIndicesOn1LevelLowerDose <- (1:NPats)[AppliedDoses==Level1LowerDose]
                # Number of patients on that dose, which can be either A or (APlusB)
                NPatsOn1LevelLowerDose <- length(PatsIndicesOn1LevelLowerDose)
                # Number of toxicity outcomes observed on that dose
                NToxsOn1LevelLowerDose <- sum(rep(1,NPatsOn1LevelLowerDose)[sapply(PatsIndicesOn1LevelLowerDose,function(x)
                  currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes == 1)])
                if (NPatsOn1LevelLowerDose==APlusB & NToxsOn1LevelLowerDose<=E) {
                  currentCTData@Conclusions <- Level1LowerDose
                  names(currentCTData@Conclusions) <- "RP2D"
                  return (list(NewCTData=currentCTData))
                }
                else{
                  NewAction1 <- new("Action",MethodCall=
                                      "generatePatientsForAccrual(
                                    popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                    OtherArgs=list(NPats=B),GlobalTime=currentGlobalTime + 1)
                  NewAction2 <- new("Action",MethodCall=
                                      "allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                    OtherArgs=list(PatsIndices=(NPats+1):(NPats+B)),GlobalTime=currentGlobalTime + 2)
                  return (list(NewActions=c(NewAction1,NewAction2)))
                }
              }
            }
            else if (NPatsOnCurrentDose==APlusB & NToxsOnCurrentDose<=E){
              if(CurrentDose==HighestAppliedDose){
                if(CurrentDose==HighestDose){
                  currentCTData@Conclusions <- NA
                  names(currentCTData@Conclusions) <- "RP2D"
                  return (list(NewCTData=currentCTData))
                }
                else{
                  NewAction1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                    OtherArgs=list(NPats=A),GlobalTime=currentGlobalTime + 1)
                  NewAction2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                    OtherArgs=list(PatsIndices=(NPats+1):(NPats+A)),GlobalTime=currentGlobalTime + 2)
                  return (list(NewActions=c(NewAction1,NewAction2)))
                }
              }
              else{
                currentCTData@Conclusions <- CurrentDose
                names(currentCTData@Conclusions) <- "RP2D"
                return (list(NewCTData=currentCTData))
              }
            }
            else if(NPatsOnCurrentDose==A & NToxsOnCurrentDose<C){
              if(CurrentDose==HighestDose){
                currentCTData@Conclusions <- NA
                names(currentCTData@Conclusions) <- "RP2D"
                return (list(NewCTData=currentCTData))
              }
              else{
                NewAction1 <- new("Action",MethodCall="generatePatientsForAccrual(popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                  OtherArgs=list(NPats=A),GlobalTime=currentGlobalTime + 1)
                NewAction2 <- new("Action",MethodCall="allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                  OtherArgs=list(PatsIndices=(NPats+1):(NPats+A)),GlobalTime=currentGlobalTime + 2)
                return (list(NewActions=c(NewAction1,NewAction2)))
              }
            }
            else {
              NewAction1 <- new("Action",MethodCall=
                                  "generatePatientsForAccrual(
                                popModelSpec=popModelSpec,nPats=NPats,currentCTData=CurrentCTData)",
                                OtherArgs=list(NPats=B),GlobalTime=currentGlobalTime + 1)
              NewAction2 <- new("Action",MethodCall=
                                  "allocateTrts(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime,patsIndices=PatsIndices)",
                                OtherArgs=list(PatsIndices=(NPats+1):(NPats+B)),GlobalTime=currentGlobalTime + 2)
              return (list(NewActions=c(NewAction1,NewAction2)))
            }
          }
) 


# It returns a list consisting of a new CT data and a list of new actions
setMethod("allocateTrts",signature(designSpec="APlusBSpecifier",currentCTData="CTData",currentGlobalTime="numeric",
                                   patsIndices="numeric"),
          function(designSpec,currentCTData,currentGlobalTime,patsIndices){
            # Total Number of patients in the current CT data
            NPats <- length(currentCTData@PatsData)
            # Number of current patients
            NCurrentPats <- length(patsIndices)
            TierDoses <- designSpec@TierDoses
            # Design parameters
            A <- designSpec@A
            B <- designSpec@B
            C <- designSpec@C 
            D <- designSpec@D 
            E <- designSpec@E
            APlusB <- A+B
            # When only first A patients have been enrolled to CT
            if (NPats == A)
              ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[1])))
            # When >A patients have been enrolled to CT
            else{
              # Dose vector for each patient that has been treated, in the ascending order of treatement times/enrollment times
              PreviousDoses <- sapply(currentCTData@PatsData[1:(NPats-NCurrentPats)],function(x) x@ConcurrentTrtsDataList[[1]]@TrtAllos$Dose)
              # Previous dose
              PreviousDose <- PreviousDoses[NPats-NCurrentPats]
              # Previous dose level
              PreviousDoseLevel <- which(TierDoses==PreviousDose)
              # Indices for patients who are on previous dose
              PatsIndicesOnPreviousDose <- (1:(NPats-NCurrentPats))[PreviousDoses==PreviousDose]
              # Number of patients on previous dose, which can be either A or (A+B)
              NPatsOnPreviousDose <- length(PatsIndicesOnPreviousDose)
              # Number of toxicity outcomes observed on previous dose
              NToxsOnPreviousDose <- sum(rep(1,NPatsOnPreviousDose)[sapply(PatsIndicesOnPreviousDose,function(x)
                currentCTData@PatsData[[x]]@ConcurrentTrtsDataList[[1]]@Outcomes == 1)])
              if ((NPatsOnPreviousDose == A & NToxsOnPreviousDose < C )|(NPatsOnPreviousDose == APlusB  & NToxsOnPreviousDose <= E))
                ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[PreviousDoseLevel+1])))
              else if (NPatsOnPreviousDose == A & NToxsOnPreviousDose >= C & NToxsOnPreviousDose <= D)
                ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=PreviousDose)))
              else ThisPatConcurrentTrtsDataList <- list(new("ConcurrentTrtsData",TrtAllos=data.frame(Dose=TierDoses[PreviousDoseLevel-1])))
            }
            for ( i in patsIndices)
              currentCTData@PatsData[[i]]@ConcurrentTrtsDataList <- ThisPatConcurrentTrtsDataList
            NewAction1 <- new("Action",MethodCall="generatePatsOutcomes(outcomeModelSpec=outcomeModelSpec,patsIndices=PatsIndices,currentCTData=CurrentCTData)",
                              OtherArgs=list(PatsIndices=patsIndices),GlobalTime=currentGlobalTime+1)
            NewAction2 <- new("Action",MethodCall="checkStoppingRule(designSpec=designSpec,currentCTData=CurrentCTData,currentGlobalTime=CurrentGlobalTime)",
                              OtherArgs=list(),GlobalTime=currentGlobalTime+2)
            return(list(NewCTData=currentCTData,NewActions=c(NewAction1,NewAction2)))
          }
)

