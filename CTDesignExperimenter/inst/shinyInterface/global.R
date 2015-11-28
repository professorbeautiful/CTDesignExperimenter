require("CTDesignExperimenter")
require(shinysky) #  needed for renderHotable, shinyalert, etc but not for ss_jstree
require(shinyBS)  #  See http://spark.rstudio.com/johnharrison/shinyBS-Demo/, 
          #  useful for many bootstrap functions including alerts, Modal and popups.
require(magrittr) ###   %>% pipe operator

options(shiny.trace=FALSE)
options(shiny.reactlog=TRUE)

scaffoldBlockNames = scaffoldObjects[[1]]

startup()

source("makeTree.R")
source("myjstree.JSON.R")
source("swapMeetObjectTables.R")


OpenSesame = '$("#jstreeScenario").jstree("open_all");'
tagToOpenTree =
  tags$script(paste0(
    'function openTree(){'
    , OpenSesame 
    , '                   }; // openTree()  works fine by hand (in evalJS box); 
       $(document).ready(    // binding only works when jstree is in ui.R, not renderUI.
            $("#jstreeScenario").ready(
              openTree));'))
