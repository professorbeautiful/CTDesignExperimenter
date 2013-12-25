###  ui.R  
###  shiny interface for defining a variable

shinyUI(bootstrapPage(
#  headerPanel(
#    tags$title("Variables")),
  mainPanel(
    helpText("Search for variables:"),
    selectInput("varChoice", "variable", output$varnames, 
                selected = NULL,
                multiple = FALSE),
    textInput(inputId="varNameInput",
              label="Variable Name", value="vname"),
    actionButton(inputId="varSearchButton",label=
                   "Search")
    , textInput(inputId="varNameOutput", label="varNameOutput")
    
  )
  
))

