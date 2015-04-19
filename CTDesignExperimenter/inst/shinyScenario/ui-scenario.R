###  ui.R  
###  shiny interface for defining a variable

makeTabPanel =  function(scafOb) {
  parseTest = paste0("tabPanel(", '"', scafOb, 
                     '", renderTable(outputID="',
                     scafOb, 
                     '")')                      
}
shinyUI(fluidPage(
  titlePanel("Scenario viewer: default scenario", "Scenario viewer"),
  uiOutput(outputId="debugTools"),  ### This is super-useful!
  navlistPanel(
    tabPanel("VIEW SCAFFOLD STRUCTURE", 
             uiOutput(outputId="scaffoldTable")),
    tabPanel("BeginSimulation", uiOutput("BeginSimulation")) ,
    tabPanel("BeginClinicalTrial", uiOutput("BeginClinicalTrial")) ,
    tabPanel("GeneratePatient", uiOutput("GeneratePatient")) ,
    tabPanel("CheckEligibility", uiOutput("CheckEligibility")) ,
    tabPanel("EnrollPatient", uiOutput("EnrollPatient")) ,
    tabPanel("AssignTreatmentPlan", uiOutput("AssignTreatmentPlan")) ,
    tabPanel("GenerateOutcomes", uiOutput("GenerateOutcomes")) ,
    tabPanel("CheckOffStudy", uiOutput("CheckOffStudy")) ,
    tabPanel("CheckModifications", uiOutput("CheckModifications")) ,
    tabPanel("SummarizePatient", uiOutput("SummarizePatient")) ,
    tabPanel("CheckStoppingRules", uiOutput("CheckStoppingRules")) ,
    tabPanel("SummarizeTrial", uiOutput("SummarizeTrial")) ,
    tabPanel("SummarizeSimulation", uiOutput("SummarizeSimulation")) 
  )
))

# Bummer, this use of evalparsetext doesn't work.

panelText = (paste0(
  'shinyUI(fluidPage(
    titlePanel("Scenario viewer: default scenario", "Scenario viewer"),
    navlistPanel(
        tabPanel("VIEW SCAFFOLD STRUCTURE", 
               uiOutput(outputId="scaffoldTable")),',
  '\n',
  paste0('tabPanel("', scaffoldObjectNames,
         '", tableOutput("', scaffoldObjectNames,
         '")) ', collapse=",\n")
  ,
  ')))'
)
)

#eval(parse(text=panelText))


