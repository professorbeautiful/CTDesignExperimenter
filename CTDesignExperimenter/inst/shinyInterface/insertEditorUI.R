output$insertEditorUI = renderUI({ 
  theInsert = findObjectInScenario(rValues$treeSelectionIndex)
  catn("output$insertEditorUI: insert is ", capture.output(theInsert))
  div(
    HTML(" Editing selected Insert - NOT IMPLEMENTED"), 
    hr(),
    actionButton(inputId="btnSearchInsert" , 
                 label="Search for insert", css.class = "treeClass-2"),
    actionButton(inputId="btnSaveInsert" , 
                 label="Save insert", css.class = "treeClass-2")
  )
})

