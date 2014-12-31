
##  createSwapMeetObjectTable  must be sourced in the renderUI call.
## Set the  pattern, for example  "^V_" and objectTypeName for example "Variable"

catn("Beginning to source createSwapMeetObjectTable.R with objectTypeName=", objectTypeName)

theFilenames = rev(dir(swapMeetDir(), pattern = pattern))
allObjectsList = lapply(theFilenames, 
                        function(fname) {
                          tempObject = source(swapMeetDir() %&% fname, local=TRUE)$value
                          objToDataframe(tempObject)
                        })
allObjectsDF = Reduce(rbind, allObjectsList)
myRadioButtons = sapply(1:nrow(allObjectsDF),
                        function(rownum) # HTML
                          HTML("<label class=\"radio\">
                                 <input type=\"radio\" name=\"choose" %&% objectTypeName %&% "\" 
                                 id=\"choose" %&% objectTypeName %&% "\"" %&% rownum
                               %&% "\" value=\"" %&% rownum %&% "\" >"
                               %&% "<span>" %&% rownum %&% "</span>"
                               %&% "</label>"
                          )
)

catn("createSwapMeetObjectTable.R:  objectTypeName=", objectTypeName,
     " : finished myRadioButtons")


assign("all" %&% objectTypeName %&% "sDF", data.frame(select=myRadioButtons, allObjectsDF),
       pos=1) 

catn("assigning ", "all" %&% objectTypeName %&% "sDF")
catn(" dim is ",
    dim(get("all" %&% objectTypeName %&% "sDF", pos=1)))

# allVarnames <<- data.frame(name=unique(allObjectsDF$name))

theObjectTable <<- renderDataTable(
  get("all" %&% objectTypeName %&% "sDF", pos=1),
  options=list(
    initComplete = I("function(oSettings, json) { console.log('Done.'); }")
    , rowCallback= I(   
      #This callback allows you to 'post process' each row after it have
      #been generated for each table draw, but before it is rendered into
      #the document.
      " function(row, data) {
                    $(row).on('click', function() {
                      console.log('Row Clicked. ', 
                        this, data, data[6]);
                      $(row).bgColor = '#131';
                      window.Shiny.shinyapp.$values['fileToLoad'] = data[6];
                      //row.addClass('rowClicked'); //not needed.
                    });
                    window.DollarRow = $(row);
                    window.DTrow = row;
                    window.DTdata = data;
                    console.log('rowCallback is complete');
                  }"
    ) ### // OK this works, but how to read 'fileToLoad' from R?
  )
)  # End of renderDataTable()

# print(theObjectTable)

if(objectTypeName=="Variable")
  output$allVariablesTable = theObjectTable

if(objectTypeName=="Insert")
  output$allInsertsTable = theObjectTable

if(objectTypeName=="Scenario")
  output$allScenariosTable = theObjectTable


 # End of createSwapMeetObjectTable()
