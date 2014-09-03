

require(shiny)
require(shinysky)
require(shinyTable)
require("CTDesignExperimenter")

shinyServer(function(input, output, session) {
  
  source("debugTools.R", local=TRUE)
  
  rValues = reactiveValues()
  rValues$openingVariableEditor = FALSE
  rValues$openingInsertEditor = FALSE
  
  source("varEditorUI.R", local=TRUE)
  source("insertEditorUI.R", local=TRUE)
  
  observeBtnEditVariable = observe(label="observeBtnEditVariable", {
    if(input$btnEditVariable > 0) {
      isolate(rValues$openingVariableEditor <- TRUE)
    }
  }
  )
  
  observeBtnEditInsert = observe(label="observeBtnEditInsert", {
    if(input$btnEditInsert > 0) {
      isolate(rValues$openingInsertEditor <- TRUE)
    }
  }
  )
  
  editingVariableObserver = observe(label="editingVariableObserver", {
    catn("editingVariableObserver: rValues$openingVariableEditor = ", rValues$openingVariableEditor)
    if(rValues$openingVariableEditor) {
      updateTabsetPanel(session, "tabsetID", selected = "Editors")
    }    #"selected" is the title on the tab.
  }
  )
  editingInsertObserver = observe(label="editingInsertObserver", {
    catn("editingInsertObserver: rValues$openingInsertEditor = ", rValues$openingInsertEditor)
    if(rValues$openingInsertEditor) {
      updateTabsetPanel(session, "tabsetID", selected = "Editors")
    }    #"selected" is the title on the tab.
  }
  )
  
  observeTabReset = observe(label = "observeTabReset", {
    cat("observed:  Resetting the tabset to ", input$tabsetID, "\n")
    if(input$tabsetID != "Editors") {    ## react if tab changes
      rValues$openingVariableEditor <- FALSE
      rValues$openingInsertEditor <- FALSE
    }
  })
  
  treeObserver = observe(
    #     observers use eager evaluation; as soon as their dependencies change, they
    #     schedule themselves to re-execute.
    label="myTreeObserver", {
      cat("treeObserver: tabsetID = ", isolate(input$tabsetID), "\n")
      input$jstreeScenario  ### Added to restore reactivity. Necessary! (a mystery)
      if(isolate(input$tabsetID) == "Current scenario") { ### Fixes part of the problem
        nColumnsInTreeValue = 6
        if(length(input$jstreeScenario) > 0) {
          nSelected <<- length(input$jstreeScenario) / nColumnsInTreeValue
          rValues$nSelected <<- nSelected
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
          rValues$treeSelectionIndex = paste(treeSelection[ , "index"], collapse=" & ")
          rValues$treeSelectionDepth = 
            length(strsplit(split = "_",
                            treeSelection[ 1, "index"]) [[1]]) - 1
          rValues$openingVariableEditor = 
            (rValues$treeSelectionDepth == 3 & rValues$nSelected == 1) 
          if(rValues$openingVariableEditor) 
            rValues$theVar = findObjectInScenario(rValues$treeSelectionIndex)
          # We do NOT want the insert editor to open automatically
          rValues$clickedOnInsert = 
            (rValues$treeSelectionDepth == 2 & rValues$nSelected == 1) 
          if(rValues$clickedOnInsert) 
            rValues$theInsert = findObjectInScenario(rValues$treeSelectionIndex)
#           rValues$openingInsertEditor = 
#             (rValues$treeSelectionDepth == 2 & rValues$nSelected == 1) 
#           if(rValues$openingInsertEditor) 
#             rValues$theInsert = findObjectInScenario(rValues$treeSelectionIndex)
        }
        else {
          rValues$treeSelectionText = ""
          rValues$treeSelectionIndex = ""
          rValues$treeSelectionDepth = 0
        }
      }
    }
  )
  # We need the following to make the values available to JS.  
  # See nodeDepthJSfunction etc.
  output$treeSelectionText = renderText(rValues$treeSelectionText)
  output$treeSelectionIndex = renderText(rValues$treeSelectionIndex)
  output$treeSelectionDepth = renderText(rValues$treeSelectionDepth)
  output$openingVariableEditor = renderText(rValues$openingVariableEditor)
  output$openingInsertEditor = renderText(rValues$openingInsertEditor)
  # treeObserver$onInvalidate(function() print("jstreeScenario selection changed!"))
  
  output$selectedNode = renderText({
    print(paste0("selectedNodes ", paste(input$jstreeScenario, collapse = ", ")))
  })
  output$selectedNodes = renderText({  ## Must have a distinct name!
    print(paste0("selectedNodes ", input$jstreeScenario, collapse = ", "))
  })
  
  popupInsertEditor = function() {
    cat("popupInsertEditor is called\n")
  } # place holder
  popupVariableEditor = function() {
    cat("popupVariableEditor is called\n")
  } # place holder
  
  onNodeClick = observe(label="onNodeClick",
                          {
                            if(rValues$treeSelectionDepth == 2) {
                              popupInsertEditor()
                            }
                            if(rValues$treeSelectionDepth == 3) {
                              popupVariableEditor()
                            }
                          })
  
  
  output$oneRunResults = renderUI({
    input$btnRunOne ## to kick it off.
    runTrial()
    nPatients = length(trialData$patientData)
    returnvalueString1 = paste0(
      "tagList(",
      "div(",
      paste("'", capture.output(
        printVVenv(trialData$trialSummaries))
        , "'", collapse=","),
      "))")
    returnvalue = eval(parse(text=returnvalueString1))
    
    for(iPatient in 1:nPatients) {
      returnvalueString2 = paste0(
        "tagList(hr(), ",
        "p(style='fontsize:large', em('Patient #",
        iPatient,
        "')), ",
        "hr(), ",
        paste("div('", capture.output(
          printVVenv(trialData$patientData[[iPatient]]$VVenv))
          , "')", collapse=", \n") ,
        ")")
      returnvalue = tagList(
        returnvalue , eval(parse(text=returnvalueString2))
      ) 
    }
    returnvalue
  })
  
  is_needed = function()
    return (grep(rValues$treeSelectionText, 'needs:') > 0 )
  is_code = function()
    return (grep(rValues$treeSelectionText, 'generator code:') > 0 )
  is_param = function()
    return (grep(rValues$treeSelectionText, 'param:') > 0 )
  is_provision = function()
    return (grep(rValues$treeSelectionText, 'provides:') > 0 )
  
  observeBtnAddScen = observe(label="observeBtnAddScen", {
    if(input$btnAddScen > 0) { ### Make reactive to button.
      updateTabsetPanel(session, "tabsetID", selected = "Experiment")
      catn("==== doing updateTabsetPanel to Experiment")
    }
  })  
  
  output$experimentTableOut = renderTable({
    if(input$btnAddScen>0) {  ### Make reactive to button. Trigger if clicked.
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
  
  observe({
   if( input$btnCloneScen > 0) {   # Trigger if clicked
    cat("\nSaving scenario\n")
    assign(isolate(input$scenarioName), pos = 1,
           currentScenario
           ##TODO: update currentScenario 
           ## responding to deletes, insertions, edits in place.
    )
    showshinyalert(session, id="cloneScen", styleclass = "inverse",
                   HTMLtext=paste(
                     "Saving scenario, name = ",
                     isolate(input$scenarioName)))
    #window.prompt("sometext","defaultText");
  }
  })
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

observe({
  if(exists("input")) 
    if(!is.null(input$btnRemoveInsert) & (input$btnRemoveInsert > 0)) {
      # Find in the Scenario object and delete and reconstruct the tree.
      # This is the actual object (e.g. VG): findObjectInScenario(rValues$treeSelectionIndex)
      selectedInsertIndex <<- which(sapply(currentScenario@inserts, function(INSERT) 
        identical(INSERT, findObjectInScenario(rValues$treeSelectionIndex,
                                               currentScenario))))
      currentScenario@inserts <<- 
        new('ListOfInserts', currentScenario@inserts[-selectedInsertIndex])
      # error:assignment of an object of class “list” is not valid for @‘inserts’
      # in an object of class “Scenario”; is(value, "ListOfInserts") is not TRUE
      reloadScenario()
    }
})