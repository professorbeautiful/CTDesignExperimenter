varToDataframe = function(theVar){
  data.frame(
    name=theVar@name,
    description=theVar@description,
    checkDataType = printFunctionBody(theVar@checkDataType),
    author=theVar@author,
    timestamp=capture.output(theVar@timestamp),
    filename=theVar@filename      
  )
}

makeTemplateVariable = function() 
  Variable(name = "(name)", description = "(description)", checkDataType = function(x)is.boolean(x))

output$varEditorUI = renderUI({ 
  rValues$openingVariableEditor <<- FALSE
  if(is.null(rValues$theVar))
    rValues$theVar = makeTemplateVariable()
  theVar = rValues$theVar
  CHECK = printFunctionBody(theVar@checkDataType)
  catn("output$varEditorUI: var is ", capture.output(theVar))
  vFilenames <<- rev(dir(swapMeetDir(), pattern = "^V_"))
  allVariablesList = lapply(vFilenames, function(fname) {
    tempVar = source(swapMeetDir() %&% fname, local=TRUE)$value
    varToDataframe(tempVar)
  })
  allVariablesDF = Reduce(rbind, allVariablesList)
  radioButtons = sapply(1:nrow(allVariablesDF),
                        function(rownum)
                          HTML("<input type=\"radio\" name=\"chooseVariable\" 
                id=\"chooseVariable" %&% rownum
                               %&% "\" value=" %&% rownum %&% ">"))
  allVariablesDF = data.frame(select=radioButtons, allVariablesDF) 
  
  allVarnames = data.frame(name=unique(allVariablesDF$name))
  
  output$allVariablesTable <<- renderDataTable(allVariablesDF,
        options=list(
            initComplete = I("function(oSettings, json) {
                                    //alert('Done.');
                                    console.log('Done.');
                                    }")
           # fnInitComplete works.
          , rowCallback= I(   
            #This callback allows you to 'post process' each row after it have
            #been generated for each table draw, but before it is rendered into
            #the document.
              " function(row, data) {
                    $(row).on('click', function() {
                      console.log('Row Clicked. ', 
                        this, data, data[5]);
                      $(row).bgColor = '#131';
                      window.Shiny.shinyapp.$values['fileToLoad']
                         = data[5];
                      //row.addClass('rowClicked');
                    });
                    window.DollarRow = $(row);
                    window.DTrow = row;
                    window.DTdata = data;
                    console.log('rowCallback is complete');
                  }"
              )
        )
#               , callback="function(oTable) {
#                     // This approach does not get me the cell info.
#                     oTable.on('click', function(el) {
#                       window.thisRow = el;
#                       //thisRow.currentTarget is HTMLTableElement
#                       //thisRow.currentTarget.children[0].innerHTML It's the header row!
#                       //alert('Row clicked.' + this); // HTMLTableElement
#                       //this.parent().find('td').each(function() {
#                       //  console.log(this.html());
#                       });
#                    });
#                }"
                           #                    
                          #                    // Cell click
                          #                    $('td', nRow).on('click', function() {
                          #                      console.log('Col Clicked.', this, aData, iDisplayIndex, iDisplayIndexFull);
                          #                    });
  )  # End of renderDataTable()
  
  #                ,
  #                callback = 'function(oTable) {
  #                   /*"initComplete": function () {*/
  #                       var api = this.api();
  #                       api.$("td").click( function () {
  #                           
  #                           api.search( this.innerHTML ).draw();
  #                           input.selectedTableRow = api.search( this.innerHTML );
  #                       } );
  #                     }
  #                   } 
  #                 ' )

  ### RETURN VALUE FOR UI
  div(
    HTML(" Variable Editor "),
    hr(),
    div(class='col-6',
        actionButton(inputId="btnNewVar" , 
                     label="New variable", css.class = "treeClass-3"),
        actionButton(inputId="btnSearchVar" , 
                     label="Find and load variable", css.class = "treeClass-3"),
        actionButton(inputId="btnSaveVar" , 
                     label="Save variable in scenario", css.class = "treeClass-3"),
        actionButton(inputId="btnSaveVarAs" , 
                     label="Save variable as...", css.class = "treeClass-3"),
        hr(),
        #                      textInput.typeahead(id="searchTypeAhead", "Search typeahead var name",
        #                                          local=allVarnames,
        #                                          tokens=1:nrow(allVarnames), #gsub("V_", "", vFilenames),
        #                                          valueKey="name", 
        #                                          template=HTML('<p>{{name}}</p>')
        #                      )
        #   textInput.typeahead(id="searchTypeAhead", "LABEL", 
        #                     local=allVariablesDF, 
        #                     tokens=paste(allVariablesDF$name,
        #                                  allVariablesDF$description), 
        #                     valueKey="filename", 
        #                     template="<p style='width:2000px'>{{name}} : {{description}}")
        #       textInput.typeahead(id="searchTypeAhead", "Search typeahead",
        #                           local=allVariablesDF,
        #                           tokens=1:nrow(allVariablesDF), #gsub("V_", "", vFilenames),
        #                           valueKey="filename", 
        #                           template=
        #                             #tagAppendAttributes(
        #                               '<p style="width:100%">{{filename}} :  {{description}}</p>' #) 
        # select2Input WORKS, and variable loads, but not what we want.
        #           select2Input("varSearchFileInput", "varSearchFileInput",
        #                      #selectize=FALSE,
        #                      choices=vFilenames,
        #                      style="width:100%"
        #                      )
        #fileInput("varSearchFileInput", "varSearchFileInput"))
        hr(),
        textInput("varName", label = "name", theVar@name),
        tagAppendAttributes(div(
          textInput("varDescription", label = "description", theVar@description)),
          style="width:100%"),
        textInput("varCHECK", label = "check", printFunctionBody(theVar@checkDataType)),
        hr(),
        renderText({"author: " %&% theVar@author}),
        renderText({"timestamp: " %&% capture.output(theVar@timestamp)}),
        renderText({"file: " %&% theVar@filename})
    ),
    div(class='col-6',
        conditionalPanel("input.btnSearchVar > 0", 
                         hr(),
                         dataTableOutput("allVariablesTable"))
    )
  )
})

observe(label="searchVariableObserver", {
  catn("searchVariableObserver: input$btnSearchVar = /", 
       capture.output(input$btnSearchVar), "/")
  if(!is.null(input$btnSearchVar))
    if(input$btnSearchVar > 0) 
      if(!is.null(input$searchTypeAhead)) 
        if(input$searchTypeAhead != "") {
          theVar = try(source(swapMeetDir() %&% input$searchTypeAhead, 
                              local = TRUE)$value) 
          if(class(theVar) == "Variable") rValues$theVar = theVar
          else shinyalert("Sorry, it wasn't a Variable file.")
        }
})


observe({       ### Find and load a variable from a file.
  if(input$tabsetID=="Editors" & !is.null(input$btnSearchVar)){
    if(input$btnSearchVar > 0 & !is.null(input$varSearchFileInput)) {
      try({
        #         fileInfo = input$varSearchFileInput
        #         print(fileInfo)
        #        theVar = source(fileInfo$datapath, local = TRUE)$value
        theVar = source(swapMeetDir() %&% input$varSearchFileInput, 
                        local = TRUE)$value
        print(str(theVar))
        if(class(theVar) == "Variable") rValues$theVar = theVar
        else shinyalert("Sorry, it wasn't a Variable file.")
      })
    }
  }
})


observe({       ### Clear the inputs to create a new variable.
  if(input$tabsetID=="Editors" & !is.null(input$btnNewVar)){
    if(input$btnNewVar > 0) {
      rValues$theVar = 
        Variable(name = "", description = "", checkDataType = function(x){TRUE})
    }
  }
})

observe({       ### Save Variable in a swapMeet file.
  if(input$tabsetID=="Editors" & !is.null(input$btnSaveVarAs)){
    if(input$btnSaveVarAs > 0) {
      theVar = try(
        Variable(name = input$varName, 
                 description = input$varDescription, 
                 checkDataType = eval(parse(text=input$varCHECK)))) 
      if(class(theVar) == "try-error")
        shinyalert("Error in variable: " %&% theVar)
      else {
        theVar = writeSwapMeetFile(theVar, verbose = TRUE)
        rValues$theVar = theVar
      }
    }
  }
})


observerChooseVariable = observe({
  chooseVariable = input$chooseVariable ## reactivity here 
  if(!is.null(chooseVariable))
    isolate({
      varFileName = rValues$allVariablesDF[input$chooseVariable, "filename"]
      catn("varFileName = ", varFileName)
      theVar = try(
        source(swapMeetDir() %&% varFileName, local=TRUE)$value
      )
      if(class(theVar) != "try-error")
        rValues$theVar <- theVar
    })
})