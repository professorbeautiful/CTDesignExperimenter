output$scenarioSearchTable = renderUI({
  
  pattern = "^S_"
  objectTypeName="Scenario"
  source("createSwapMeetObjectTable.R", local=TRUE)  
  
  observeChooseScenario = observe(label="observeChooseScenario", {
    chooseScenario = input$chooseScenario ## reactivity here 
    catn("observerChooseScenario:  chooseScenario number= ", chooseScenario)
    if(!is.null(chooseScenario))
      isolate({
        scenarioFileName = allScenariosDF[chooseScenario, "filename"]
        catn("observerChooseScenario:  FileName = ", scenarioFileName)
        currentScenario = try(
          source(getSwapMeetDir() %&% scenarioFileName, local=TRUE)$value
        )
        if(class(currentScenario) != "try-error")
          rValues$currentScenario <- currentScenario
      })
  })
  
  div(class='col-6',
      conditionalPanel(
        "input.btnSearchScenario > 0", 
        hr(),
        h3("SEARCHING SCENARIOS"),
        tagAppendAttributes(a(""), id="idSearchScenario"),
        h3("Click on the radiobutton to load the Scenario into the template above."),
        HTML('<div id="chooseScenario" class="control-group shiny-input-radiogroup">
                   <label class="control-label" for="chooseScenario">Swapmeet Scenarios</label>'),
        dataTableOutput("allScenariosTable"),
        HTML('</div>')
      )
  )
})