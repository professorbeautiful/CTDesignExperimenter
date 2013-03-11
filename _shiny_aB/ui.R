require(RBioinf)
require(RJSONIO)

shinyUI(pageWithSidebar(  
	header="header",
	sidebarPanel=sidebarPanel(
		actionButton("aB","aBlabel")
	)
	 , mainPanel=mainPanel(
     "before",
		textOutput(outputId="aBtext"),
     "after"
	)
))
