

require(shiny)
require(shinysky)
require(shinyTable)
require("CTDesignExperimenter")

shinyServer(function(input, output, session) {
  
  source("debugTools.R", local=TRUE)
  
  source("varEditorUI.R", local=TRUE)

  rValues = reactiveValues()
  rValues$editingVariable = FALSE
  
  observe({
    if(input$btnEditVariable > 0) {
      rValues$editingVariable == TRUE
    }
  }
  )
  
#   output$varEditPopup <- renderPrint({
#     code <- input$console
#     output <- eval( parse( text=code ) )
#     return(output)
#   })
  
  observe(label="editingVariableObserver", {
    catn("editingVariableObserver: rValues$editingVariable = ", rValues$editingVariable)
    if(rValues$editingVariable) {
      updateTabsetPanel(session, "tabsetID", selected = "Editors")
    }
    #"selected" is the title on the tab.
  }
  )
  
  
  treeObserver = observe(
    #     observers use eager evaluation; as soon as their dependencies change, they
    #     schedule themselves to re-execute.
    label="myTreeObserver", {
      nColumnsInTreeValue = 6
      if(length(input$jstree1) > 0) {
        nSelected <<- length(input$jstree1) / nColumnsInTreeValue
        rValues$nSelected <<- nSelected
        treeSelection <<- matrix(ncol=nColumnsInTreeValue, input$jstree1, byrow=T,
                                 dimnames=list(1:nSelected,
                                               names(input$jstree1)[1:nColumnsInTreeValue]))
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
        rValues$editingVariable = 
          (rValues$treeSelectionDepth == 3 & rValues$nSelected == 1)    
      }
      else {
        rValues$treeSelectionText = ""
        rValues$treeSelectionIndex = ""
        rValues$treeSelectionDepth = 0
      }
    }
  )
  # We need the following to make the values available to JS.  
  # See nodeDepthJSfunction etc.
  output$treeSelectionText = renderText(rValues$treeSelectionText)
  output$treeSelectionIndex = renderText(rValues$treeSelectionIndex)
  output$treeSelectionDepth = renderText(rValues$treeSelectionDepth)
  output$editingVariable = renderText(rValues$editingVariable)
  # treeObserver$onInvalidate(function() print("jstree1 selection changed!"))
  
  output$selectedNode = renderText({
    print(paste0("selectedNodes ", paste(input$jstree1, collapse = ", ")))
  })
  output$selectedNodes = renderText({  ## Must have a distinct name!
    print(paste0("selectedNodes ", input$jstree1, collapse = ", "))
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
  
  observe({
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
