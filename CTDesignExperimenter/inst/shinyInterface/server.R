

require(shiny)
#require(shinysky)
#require(shinyTable)
require("CTDesignExperimenter")


shinyServer(function(input, output, session) {
  
  thisSession <<- session
  
  observerCurrentScenario = observe({
    currScen = rValues$currentScenario
    messageSent = 
      myjstree.JSON(
        makeTree(scenario=currScen, "full")
      )
    # messageSent = tags$ul(messageSent)
    #messageSent = jstree("jstreeScenario",  messageSent)
    messageSent = capture.output(  print(messageSent))
    messageSent = paste(collapse=" ", messageSent)
    messageSent <<- messageSent
    
    #newTree = '<ul> B      <li> B-A         <ul>            <li>       B-A-A          </li><li>             B-A-B </li>        </ul></li>      <li> B-B </li>    </ul>';
    
    # messageSent = messageSent [[5]] [[3]]
    #   messageSent [[5]] [[3]] is the html
    #   In ss-jstree.ss receiveMessage, 
    #   messageSent[[5]][[3]]
    
    session$sendInputMessage("jstreeScenario", messageSent)
    
    #session$sendInputMessage("jstreeScenario", newTree)
    catn("str of messageSent[[5]][[3]] is ", str(messageSent))
  })
  
  source("debugTools.R", local=TRUE)
  
  rValues = reactiveValues()
  
  rValues$currentScenario = defaultScenario
  
  ### Without this "reactive" wrapper, I get the error 
  # Error in .getReactiveEnvironment()$currentContext() : 
  #   Operation not allowed without an active reactive context. (You tried to do something that can only be done from inside a reactive expression or observer.)
  #   rValues$scenarioTree = reactive({
  #     catn("reloadScenario, length of inserts is ",
  #         length(rValues$currentScenario@inserts))
  #     makeTree(scenario=rValues$currentScenario, "full")
  #     #catn("length of scenarioTree is ", length(rValues$scenarioTree))
  #     # length(scenarioTree) is 13
  #   })
  
  # myTree was initially created in global.R. 
  #   Must wrap this in "reactive", or else "Operation no allowed withou an active reacive context..."
  #   rValues$myTree =  reactive({
  #   catn("Changing myTree in reactive expression. Number of inserts is ",
  #        length(rValues$currentScenario@inserts))
  #   jstree("jstreeScenario",  myjstree.obj(
  #     makeTree(scenario=rValues$currentScenario, "full")))
  # })
  
  output$jstreeScenarioOutput = renderUI({
    ##rValues$myTree  ## No error message here.
    catn("Changing myTree in reactive expression. Number of inserts is ",
         length(rValues$currentScenario@inserts))
    div(style="overflow:scroll;height:400px;",
        myListToJStreeTags("jstreeScenario",  
                           myjstree.obj(
                             makeTree(scenario=rValues$currentScenario, "full")))
        , singleton(tagToOpenTree) ### does not help
        , singleton(tags$script(OpenSesame) )  ### does not help
    )
  })
  
  #  
  #   observe({
  #     catn("class of myTreeObj is ", class(rValues$myTreeObj))
  #   })
  #   output$myTree = jstree(id="jstreeScenario", rValues$myTreeObj)
  #   output$TEMP = renderText({"TEMP"})
  # class(renderText({"TEMP"}))  is   shiny.render.function function
  #   output$myTree = renderUI({
  #     rValues$myTreeObj # temporary; make sure it's reactive
  #     catn("renderUI for myTree")
  #      returnvalue = jstree(id="jstreeScenario", rValues$myTreeObj)
  #      catn("renderUI for myTree: class of returnvalue is ", class(returnvalue))
  #      #div(returnvalue)
  #"jstree HERE"
  #     textOutput('TEMP')
  # class(renderText({"TEMP"}))  is   shiny.render.function function
  #   })
  ## components of output$:
  # window.Shiny.shinyapp.$bindings.myTree.el  is HTMLDivElement
  # so is window.Shiny.shinyapp.$bindings.treeSelectionText.el
  #window.Shiny.shinyapp.$bindings.treeSelectionText.el.attributes.length is 2
  # From window.Shiny.shinyapp.$bindings.myTree.el.attributes.class.value, we get
  #          shiny-html-output shiny-bound-output shiny-output-error
  # Handy code:  pbcopy(capture.output(myTree)); sink()
  
  #   vgNodes = unlist( 
  #     traverse(myTree, callback = 1, searchTerm = "vg_")
  #     )
  #   vgNodeIndices = sapply(strsplit(vgNodes, x = " "), ''
  #   for(node in vgNodes) {
  #     locationVector = strsplit(vgNodes)
  #     myTree[[locationVector]] <-
  #       tagAppendAttributes(myTreeTemp[[locationVector]], class="CLASS")
  #   }
  # length(unlist(myTree)) is 192.  Very useful names! Gives depth.
  # as.vector(unlist(myTree))
  
  # table(unlist(myTree)[grep("name", names(unlist(myTree)))])
  # div   head     li   link script     ul 
  #   1      3     60      1      3     18 
  #unlist(myTree)[which(unlist(myTree) == "link") + (0:3)]
  #unlist(myTree)[which(unlist(myTree) == "") + (0:3)]
  
  # 
  #  unlist(myTree)[(grep("(ec|vg)_", unlist(myTree)))]  ### 11 vg or ec.
  ## All are children.children.children.children.children
  #  unlist(myTree)[(grep("v_", unlist(myTree)))]  #NONE.
  # Using opm:traverse
  
  # traverse = function(li, func) {
  #   if(is.list(li) return(lapply()))
  # }
  
  ## Start with current Scenario.
  rValues$currentScenario = defaultScenario  
  #  reloadScenario()
  
  
  rValues$openingVariableEditor = FALSE
  rValues$openingInsertEditor = FALSE
  
  wasClicked =  function(button) {
    if(exists("input"))  
      if(!is.null(button) ) {
        if(button > 0) {        
          return(TRUE)
        }
      }
    return(FALSE)
  }
  
  source("varEditorUI.R", local=TRUE)
  source("insertEditorUI.R", local=TRUE)
  source("scenarioSearchTableUI.R", local=TRUE)
  #source("scenarioEditorUI.R", local=TRUE)
  
  
  
  observerBtnEditVariable = observe(label="observerBtnEditVariable", {
    if(wasClicked(input$btnEditVariable)) {
      isolate(rValues$openingVariableEditor <- TRUE)
    }
  }
  )
  
  observerBtnEditInsert = observe(label="observerBtnEditInsert", {
      if(wasClicked(input$btnEditInsert) ) {
          isolate({
            cat("Setting rValues$openingInsertEditor <- TRUE\n")
            rValues$openingInsertEditor <- TRUE
          })
        }
  }
  )
  
  observerEditingVariable = observe(label="observerEditingVariable", {
    catn("observerEditingVariable: rValues$openingVariableEditor = ", rValues$openingVariableEditor)
    if(rValues$openingVariableEditor) {
      updateTabsetPanel(session, "tabsetID", selected = "Variable Editor")
    }    #"selected" is the title on the tab.
  }
  )
  
  observerEditingInsert = observe(label="observerEditingInsert", {
    catn("observerEditingInsert: rValues$openingInsertEditor = ", rValues$openingInsertEditor)
    if(rValues$openingInsertEditor) {
      updateTabsetPanel(session, "tabsetID", selected = "Insert Editor")
    }    #"selected" is the title on the tab.
  }
  )
  
  observerClickedParameter = observe(label="observerClickedParameter", 
                                     { f.clickedParameter() })
  f.clickedParameter <<- function() {
    if(isTRUE(rValues$clickedOnParameter)) {
      catn("observerClickedParameter: rValues$treeSelectionText = ", rValues$treeSelectionText)
      rValues$treeSelectionPath = substr(rValues$treeSelectionPath, 1, 5) ## Point to the insert instead of the parameter
      catn("observerClickedParameter(1): rValues$treeSelectionPath = ", rValues$treeSelectionPath)
      rValues$theInsert = findObjectInScenario(rValues$treeSelectionPath, scenario=rValues$currentScenario)
      updateTabsetPanel(session, "tabsetID", selected = "Insert Editor")
      catn("observerClickedParameter(2): rValues$treeSelectionPath = ", rValues$treeSelectionPath)
      rValues$clickedOnParameter = FALSE
    }
  }
  
  
  observerClickedGenerator = observe(label="observerClickedGenerator", {
    if(isTRUE(rValues$clickedOnGenerator)) {
      catn("observerClickedGenerator: rValues$treeSelectionText = ", rValues$treeSelectionText)
      rValues$treeSelectionPath = substr(rValues$treeSelectionPath, 1, 
                                         gregexpr("_", rValues$treeSelectionPath)[[1]][3]-1)
      catn("observerClickedGenerator(1): rValues$treeSelectionPath = ", rValues$treeSelectionPath)
      rValues$theInsert = findObjectInScenario(rValues$treeSelectionPath, scenario=rValues$currentScenario)
      updateTabsetPanel(session, "tabsetID", selected = "Insert Editor")
      catn("observerClickedGenerator(2): rValues$treeSelectionPath = ", rValues$treeSelectionPath)
      rValues$clickedOnGenerator = FALSE
    }
  }
  )
  
  observeTabReset = observe(label = "observeTabReset", {
    cat("observed:  Resetting the tabset to ", input$tabsetID, "\n")
    if(input$tabsetID != "Variable Editor")     ## react if tab changes
      rValues$openingVariableEditor <- FALSE
    if(input$tabsetID != "Insert Editor")     ## react if tab changes
      rValues$openingInsertEditor <- FALSE
  })
  
  treeObserver = observe(
    #     observers use eager evaluation; as soon as their dependencies change, they
    #     schedule themselves to re-execute.
    label="myTreeObserver", {
      cat("treeObserver: tabsetID = ", isolate(input$tabsetID), "\n")
      input$jstreeScenario  ### Added to restore reactivity. Necessary! (a mystery)
      
      if(isolate(input$tabsetID) == "Current scenario") { ### Fixes part of the problem
        nColumnsInTreeValue = 6  ### 7 if using shinyTree
        if(length(input$jstreeScenario) > 0) {
          nSelected <<- length(input$jstreeScenario) / nColumnsInTreeValue
          rValues$nSelected <<- nSelected
          result = try({
            treeSelection <<- matrix(ncol=nColumnsInTreeValue, input$jstreeScenario, byrow=T,
                                     dimnames=list(1:nSelected,
                                                   names(input$jstreeScenario)[1:nColumnsInTreeValue]))
            ## Trim leading and trailing whitespace.
            treeSelection[ , "text"] <<- gsub("^[\n\t ]*", "",
                                              gsub("[\n\t ]*$", "",
                                                   treeSelection[ , "text"] ))
            cat("Entered treeObserver. rValues$treeSelection is:\n")
            print(treeSelection)
            rValues$treeSelectionText = paste(treeSelection[ , "text"], collapse=" & ")
            rValues$treeSelectionPath = paste(treeSelection[ , "pathAttr"], collapse=" & ")
            rValues$treeSelectionDepth = 
              length(strsplit(split = "_",
                              treeSelection[ 1, "pathAttr"]) [[1]]) - 1
            rValues$openingVariableEditor = 
              (rValues$treeSelectionDepth == 3  
                 & ( identical(1, grep("^provides:|^needs:", 
                                     rValues$treeSelectionText)))
                 &  rValues$nSelected == 1) 
            if(rValues$openingVariableEditor) 
              rValues$theVar = findObjectInScenario(rValues$treeSelectionPath, scenario=rValues$currentScenario)

            rValues$clickedOnGenerator = 
              (rValues$treeSelectionDepth == 3  
               & ( identical(1L, grep("^code:", 
                                     rValues$treeSelectionText)))
               &  rValues$nSelected == 1) 
            
            rValues$clickedOnParameter = 
              (rValues$treeSelectionDepth == 3  
               & ( identical(1L, grep("^param:", 
                                     rValues$treeSelectionText)))
               &  rValues$nSelected == 1) 
            
            rValues$clickedOnInsert = 
              (rValues$treeSelectionDepth == 2 & rValues$nSelected == 1) 
            if(rValues$clickedOnInsert) 
              rValues$theInsert = findObjectInScenario(rValues$treeSelectionPath, scenario=rValues$currentScenario)
            # When an insert is clicked,
            # we do NOT want the insert editor to open automatically <<<=====
            #  Instead, we provide custom buttons.
          })
          if(class(result) ==  'try-error') {
            cat("problem with treeSelection: ", input$jstreeScenario, "\n")
          }
        }
        else {
          rValues$treeSelectionText = ""
          rValues$treeSelectionPath = ""
          rValues$treeSelectionDepth = 0
        }
      }
    }
  )
  # We need the following to make the values available to JS.  
  # See nodeDepthJSfunction etc.
  output$treeSelectionText = renderText(rValues$treeSelectionText)
  output$treeSelectionPath = renderText(rValues$treeSelectionPath)
  output$treeSelectionDepth = renderText(rValues$treeSelectionDepth)
  output$openingVariableEditor = renderText(rValues$openingVariableEditor)
  output$openingInsertEditor = renderText(rValues$openingInsertEditor)
  # treeObserver$onInvalidate(function() print("jstreeScenario selection changed!"))
  
  output$SCENARIO_TREE_label = renderText({
    ("SCENARIO TREE" %&% ifelse(isTRUE(rValues$treeSelectionText != ""),
                                      " (click here to clear selection)", ""))
  })
  
  output$selectedNode = renderText({
    print(paste0("selectedNodes ", paste(input$jstreeScenario, collapse = ", ")))
  })
  output$selectedNodes = renderText({  ## Must have a distinct name!
    print(paste0("selectedNodes ", input$jstreeScenario, collapse = ", "))
  })
  
  
  #### Display Design Parameters.
  oneRunHeader =   function() {
    returnvalueStringDesignParameters = paste0(
      "tagList(",
      "div(h3('Trial Design Parameters'), ",
      paste("'", capture.output(
        printVVenv(trialData$designParameters))
        , "'", collapse=","),
      "))")
    returnvalueStringPopulationParameters = paste0(
      "<div> <h3>Population Parameters</h3> "
      , paste(
        gsub("\\(parameter\\)", "",
             grep(value=TRUE, "(parameter)", capture.output(
               printVVenv(trialData$candidatePatient$VVenv))))
        , collapse="<br/>")
      , "</div>")
    #    catn("returnvalueStringPopulationParameters = ", returnvalueStringPopulationParameters)
    return(tagList(
      eval(parse(text=returnvalueStringDesignParameters)),
      HTML(returnvalueStringPopulationParameters),
      hr()
    ))
  }
  
  oneRunSummaries =   function() {
    if(input$btnOneRun > 0)## to kick it off.
      runTrial()
    nPatients = length(trialData$patientData)
    catn("nPatients = ", nPatients)
    returnvalueStringTrialSummaries = paste0(
      "tagList(",
      "div(h3('Summaries of the Trial'), ",
      paste("'", capture.output(
        printVVenv(trialData$trialSummaries))
        , "'", collapse=","),
      "))")
    returnvalue = eval(parse(text=returnvalueStringTrialSummaries))
    returnvalue = tagList(returnvalue, hr(),
                          div(h3('Individual Patients (variable values)')))
    returnvalue = tagList(returnvalue, 
                          paste("# patients = ", nPatients))
    returnvalue = tagList(returnvalue, 
                          numericInput(inputId = "patientChoice", "View Patient (#)",
                                       value=1, min=1, max=nPatients, step=1))
    
    return(returnvalue)
  }
  
    
  oneRunResults =   function() {
    if(input$btnOneRun > 0)
      iPatient = input$patientChoice
    catn("iPatient = ", capture.output(iPatient))  
    if(is.null(iPatient)) 
      return("")
    else {
      returnvalueStringChosenPatient = paste0(
        "<hr/> <p style='fontsize:large'> 
        <em> ", paste('Patient #', iPatient),
        "</em> </p>",
        paste(
          gsub("\\(variable value\\)", "",
               grep(value=TRUE, "(variable value)", capture.output(
                 printVVenv(trialData$patientData[[iPatient]]$VVenv))))
          , collapse="<br/>"),
        ")")
      return(HTML(text=returnvalueStringChosenPatient))
    }
    returnvalue
  }
  #  debug(oneRunResults)
  
  output$oneRunHeader = renderUI({oneRunHeader()})
  output$oneRunSummaries = renderUI({oneRunSummaries()})
  output$oneRunResults = renderUI({oneRunResults()})

  is_needed = function()
    return (grep(rValues$treeSelectionText, 'needs:') > 0 )
  is_code = function()
    return (grep(rValues$treeSelectionText, 'generator code:') > 0 )
  is_param = function()
    return (grep(rValues$treeSelectionText, 'param:') > 0 )
  is_provision = function()
    return (grep(rValues$treeSelectionText, 'provides:') > 0 )
  
  observeBtnAddScen = observe(label="observeBtnAddScen", {
    if(wasClicked(input$btnAddScen) ) { ### Make reactive to button.
      updateTabsetPanel(session, "tabsetID", selected = "Experiment")
      catn("==== doing updateTabsetPanel to Experiment")
    }
  })  
  
  observeBtnFindScen = observe(label="observeBtnFindScen", {
    if(wasClicked(input$btnFindScen) ) { ### Make reactive to button.
      rValues$findingScenario = TRUE
      # Now, replace the tree with a scenario file table
      
    }
  })  
  
  output$experimentTableOut = renderTable({
    if(wasClicked(input$btnAddScen)) {  ### Make reactive to button. Trigger if clicked.
      experimentTable[nrow(experimentTable)+1, ] <<- NA
      catn("==== appended...")
      print(rownames(experimentTable))
      print(input$scenarioName)
      try(
        rownames(experimentTable) [nrow(experimentTable)] <<- 
          input$scenarioName
      )
      catn("==== rownames changed...")
      print(experimentTable)
    }
  })  

  observerNewScenario = observe({
    if( wasClicked(input$btnNewScenario) ) {   # Trigger if clicked
      cat("\nNew scenario\n")
      scenario = new("Scenario")
      scenario@inserts = new("ListOfInserts", list(vg_SampleSizeMax_2) )
      rValues$currentScenario = scenario
    }
  })
      
  observerSaveScenarioToGlobalEnv = observe({
    if( wasClicked(input$btnSaveScenarioToGlobalEnv) ) {   # Trigger if clicked
      cat("\nSaving scenario\n")
      assign(isolate(input$scenarioName), pos = 1,
             rValues$currentScenario
             ##TODO: update rValues$currentScenario 
             ## responding to deletes, insertions, edits in place.
      )
      shinysky:::showshinyalert(session, id="SaveScenarioToGlobalEnv", styleclass = "inverse",
                                HTMLtext=paste(
                                  "Saving scenario to GlobalEnv, name = ",
                                  isolate(input$scenarioName)))
      #window.prompt("sometext","defaultText");
    }
  })
  
  observerWriteScenarioToSwapmeet = observe({
    if( wasClicked(input$btnWriteScenarioToSwapmeet) ) {   # Trigger if clicked
      cat("\nWriting scenario\n")
      assign(isolate(input$scenarioName), pos = 1,
             rValues$currentScenario
             ##TODO: update rValues$currentScenario 
             ## responding to deletes, insertions, edits in place.
      )
      shinysky:::showshinyalert(session, id="SaveScenarioToGlobalEnv", styleclass = "inverse",
                                HTMLtext=paste(
                                  "Saving scenario to GlobalEnv, name = ",
                                  isolate(input$scenarioName)))
      #window.prompt("sometext","defaultText");
    }
  })
  
  
  ### Implement the btnRemoveInsert button.
  ### First, allow modification to the tree:
  # operation can be 'create_node', 'rename_node', 'delete_node', 'move_node' or 'copy_node'
  # in case of 'rename_node' node_position is filled with the new node name
  JSallowDeletion = tags$script("
                              $('#jstreeScenario').jstree({
                              'core' : {
                              'check_callback' : function (operation, node, node_parent, node_position, more) {
                              return operation === 'delete_node';
                              }
                              }
                              });
                              function removeNode(node) {
                              $('#jstreeScenario').jstree('delete_node', node); // e.g. #j1_2.
                              }
                              ")
  
  removeInsert = function(rVcS, treeSelectionPath) {
    cat("buttonRemoveInsertObserver: treeSelectionPath is ", treeSelectionPath, "\n")
    # Find in the Scenario object and delete and reconstruct the tree.
    # This is the actual object (e.g. VG): findObjectInScenario(rValues$treeSelectionPath)
    selectedInsertIndex <<- which(sapply(
      rVcS@inserts,
      function(INSERT) 
        identical(INSERT, findObjectInScenario(treeSelectionPath,
                                               (rVcS)))))
    if(length(selectedInsertIndex) == 1) {   
      catn("REMOVING insert # ", selectedInsertIndex, ": ",
           names(rVcS@inserts)[[selectedInsertIndex]])
      rVcS@inserts <- 
        new('ListOfInserts', (rVcS@inserts)[-selectedInsertIndex])
      catn("#inserts is now ", length(isolate(rVcS@inserts)))
    }
    else catn("ERROR in removing insert from Scenario: selectedInsertIndex is ",
              selectedInsertIndex, "\n  No insert was deleted. Still ", length((rVcS@inserts)),
              " inserts.")
    return (rVcS)
  }
  
  buttonRemoveInsertObserver = observe({
      if(wasClicked(input$btnRemoveInsert) ) {
        isolate({
          treeSelectionPath <<- isolate(rValues$treeSelectionPath)
          rVcS = rValues$currentScenario  ### Trying to prevent too much reactivity.
          rValues$currentScenario <- removeInsert(rVcS, treeSelectionPath)
          # http://stackoverflow.com/questions/11139482/how-to-refresh-a-jstree-without-triggering-select-node-again
        })
      }
  })
  
  
  
})  ### End of ShinyServer call
