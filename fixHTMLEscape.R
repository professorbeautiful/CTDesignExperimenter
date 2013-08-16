### to fix a problem where the args text euals character(0).
### 2013-08-06
### See clone at /Users/Roger/Dropbox/_HOME/R-in-Dropbox/shiny/R

assignInNamespace("htmlEscape",ns="shiny", value=
                    function(text, attribute=TRUE) {
                      catn("htmlEscape: text=", text)
                      pattern <- if(attribute)
                        .htmlSpecialsPatternAttrib 
                      else
                        .htmlSpecialsPattern
                      catn("htmlEscape: pattern=", pattern)
                      if(length(text)==0){
                        browser()
                        text = ""
                      }
                      # Short circuit in the common case that there's nothing to escape
                      if (!grepl(pattern, text))
                        return(text)
                      
                      specials <- if(attribute)
                        .htmlSpecialsAttrib
                      else
                        .htmlSpecials
                      
                      for (chr in names(specials)) {
                        text <- gsub(chr, specials[[chr]], text, fixed=TRUE)
                      }
                      
                      return(text)
                    }
)
# 
# sys.calls()
# 
# [[26]]
# renderPage(ui, textConn)
# 
# [[27]]
# tagWrite(ui, function(text) cat(text, file = textConn), 0, context)
# 
# [[28]]
# sapply(tag, function(t) tagWrite(t, textWriter, indent, context))
# 
# [[29]]
# lapply(X = X, FUN = FUN, ...)
# 
# [[30]]
# FUN(X[[2L]], ...)
# 
# [[31]]
# tagWrite(t, textWriter, indent, context)
# 
# [[32]]
# sapply(tag, function(t) tagWrite(t, textWriter, indent, context))
# 
# [[33]]
# lapply(X = X, FUN = FUN, ...)
# 
# [[34]]
# FUN(X[[1L]], ...)
# 
# [[35]]
# tagWrite(t, textWriter, indent, context)
# 
# [[36]]
# tagWrite(child, textWriter, indent + 1, context)
# 
# [[37]]
# tagWrite(child, textWriter, indent + 1, context)
# 
# [[38]]
# tagWrite(child, textWriter, indent + 1, context)
# 
# [[39]]
# tagWrite(child, textWriter, indent + 1, context)
# 
# [[40]]
# tagWrite(tag$children[[1]], textWriter, 0, context, "")
# 
# [[41]]
# textWriter(paste(indentText, normalizeText(tag), eol, sep = ""))
# 
# [[42]]
# cat(text, file = textConn)
# 
# [[43]]
# paste(indentText, normalizeText(tag), eol, sep = "")
# 
# [[44]]
# normalizeText(tag)
# 
# [[45]]
# htmlEscape(text, attribute = FALSE)
# 
# 
# Browse[1]> args(tagWrite)
# function (tag, textWriter, indent = 0, context = NULL, eol = "\n") 
#   NULL
# Browse[1]> get("tag", envir=sys.frame(35))
# htmlEscape: text= container-fluid 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= row-fluid 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= headerOutput 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= shiny-html-output 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= row-fluid 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= span4 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= well 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= control-group shiny-input-radiogroup 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= control-label 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= Choose a view 
# htmlEscape: pattern= &|<|> 
# htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice1 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= View spec classes 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= checked 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= View spec classes 
# htmlEscape: pattern= &|<|> 
# htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice2 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= View spec objects 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= View spec objects 
# htmlEscape: pattern= &|<|> 
# htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= viewChoice 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice3 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= Define one clinical trial 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= Define one clinical trial 
# htmlEscape: pattern= &|<|> 
#   htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= viewChoice4 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= Run one clinical trial 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= Run one clinical trial 
# htmlEscape: pattern= &|<|> 
# htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
#   
#   htmlEscape: text= radio 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice 
# |tmlEscape: pattern= &|<|>|'|"|
# 
# htmlEscape: text= viewChoice5 
# |tmlEscape: pattern= &|<|>|'|"|
#  
# htmlEscape: text= Do a CT experiment 
# |tmlEscape: pattern= &|<|>|'|"|
#  
# htmlEscape: text= Do a CT experiment 
# htmlEscape: pattern= &|<|> 
# htmlEscape: text=  
# htmlEscape: pattern= &|<|> 
# Called from: htmlEscape(text, attribute = FALSE)
# 
# 
# ================
# 
# 
# 
# rowse[2]> str(get("tag", envir=sys.frame(35)))
# List of 3
# $ name    : chr "div"
# $ attribs :List of 1
# ..$ class: chr "container-fluid"
# $ children:List of 2
# ..$ :List of 3
# .. ..$ name    : chr "div"
# .. ..$ attribs :List of 1
# .. .. ..$ class: chr "row-fluid"
# .. ..$ children:List of 1
# .. .. ..$ :List of 3
# .. .. .. ..$ name    : chr "div"
# .. .. .. ..$ attribs :List of 2
# .. .. .. .. ..$ id   : chr "headerOutput"
# .. .. .. .. ..$ class: chr "shiny-html-output"
# .. .. .. ..$ children: list()
# .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. ..- attr(*, "class")= chr "shiny.tag"
# ..$ :List of 3
# .. ..$ name    : chr "div"
# .. ..$ attribs :List of 1
# .. .. ..$ class: chr "row-fluid"
# .. ..$ children:List of 2
# .. .. ..$ :List of 3
# .. .. .. ..$ name    : chr "div"
# .. .. .. ..$ attribs :List of 1
# .. .. .. .. ..$ class: chr "span4"
# .. .. .. ..$ children:List of 1
# .. .. .. .. ..$ :List of 3
# .. .. .. .. .. ..$ name    : chr "form"
# .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. ..$ class: chr "well"
# .. .. .. .. .. ..$ children:List of 7
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. ..$ id   : chr "viewChoice"
# .. .. .. .. .. .. .. .. ..$ class: chr "control-group shiny-input-radiogroup"
# .. .. .. .. .. .. .. ..$ children:List of 6
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-label"
# .. .. .. .. .. .. .. .. .. .. ..$ for  : chr "viewChoice"
# .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. ..$ : chr "Choose a view"
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 5
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ type   : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ name   : chr "viewChoice"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ id     : chr "viewChoice1"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ value  : chr "View spec classes"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ checked: chr "checked"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "View spec classes"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "viewChoice"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "viewChoice2"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "View spec objects"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "View spec objects"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "viewChoice"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "viewChoice3"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "Define one clinical trial"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "Define one clinical trial"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "viewChoice"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "viewChoice4"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "Run one clinical trial"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "Run one clinical trial"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "viewChoice"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "viewChoice5"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "Do a CT experiment"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "Do a CT experiment"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "hr"
# .. .. .. .. .. .. .. ..$ attribs : NULL
# .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. ..$ : chr(0) 
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"View spec classes\""
# .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. ..$ : chr "Choose spec class to view its subclasses."
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceClasses"
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-group shiny-input-radiogroup"
# .. .. .. .. .. .. .. .. .. ..$ children:List of 6
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-label"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ for  : chr "specChoiceClasses"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "choose spec type"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 5
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type   : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name   : chr "specChoiceClasses"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id     : chr "specChoiceClasses1"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value  : chr "BaseCharModelSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ checked: chr "checked"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "patient attributes"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "specChoiceClasses"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceClasses2"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "PopModelSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "population models"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "specChoiceClasses"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceClasses3"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "OutcomeModelSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "outcome models"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "specChoiceClasses"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceClasses4"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "DesignSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "designs"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "specChoiceClasses"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceClasses5"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "EvalSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "evaluation criteria"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"View spec objects\""
# .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. ..$ : chr "Choose spec class to view its objects."
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceModels"
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-group shiny-input-radiogroup"
# .. .. .. .. .. .. .. .. .. ..$ children:List of 6
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "control-label"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ for  : chr "specChoiceModels"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "choose spec type"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 5
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type   : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name   : chr "specChoiceModels"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id     : chr "specChoiceModels1"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value  : chr "BaseCharModelSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ checked: chr "checked"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "patient attributes"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "specChoiceModels"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceModels2"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "PopModelSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "population models"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "specChoiceModels"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceModels3"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "OutcomeModelSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "outcome models"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "specChoiceModels"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceModels4"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "DesignSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "designs"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "label"
# .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ class: chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "input"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs :List of 4
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ type : chr "radio"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name : chr "specChoiceModels"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "specChoiceModels5"
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ value: chr "EvalSpecifier"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ name    : chr "span"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..$ : chr "evaluation criteria"
# .. .. .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"Define one clinical trial\""
# .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. ..$ : chr "Spec selections for one CT:"
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "componentsForBuildingModel"
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "shiny-html-output"
# .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"Run one clinical trial\""
# .. .. .. .. .. .. .. ..$ children:List of 2
# .. .. .. .. .. .. .. .. ..$ : chr "sim1CTbuttonUI"
# .. .. .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. .. .. ..$ id   : chr "sim1CTbuttonUI"
# .. .. .. .. .. .. .. .. .. .. ..$ class: chr "shiny-html-output"
# .. .. .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"Do a CT experiment\""
# .. .. .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. .. .. ..$ : chr "This view choice is not yet implemented."
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. ..$ :List of 3
# .. .. .. ..$ name    : chr "div"
# .. .. .. ..$ attribs :List of 1
# .. .. .. .. ..$ class: chr "span8"
# .. .. .. ..$ children:List of 6
# .. .. .. .. ..$ :List of 3
# .. .. .. .. .. ..$ name    : chr "h3"
# .. .. .. .. .. ..$ attribs : list()
# .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. ..$ id   : chr "mainPanelHeader"
# .. .. .. .. .. .. .. .. ..$ class: chr "shiny-html-output"
# .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. ..$ :List of 3
# .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"View spec classes\""
# .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. ..$ id   : chr "classes_table"
# .. .. .. .. .. .. .. .. ..$ class: chr "shiny-html-output"
# .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. ..$ :List of 3
# .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"View spec objects\""
# .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. ..$ id   : chr "objects_table_1"
# .. .. .. .. .. .. .. .. ..$ class: chr "shiny-html-output"
# .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. ..$ :List of 3
# .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"Define one clinical trial\""
# .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. ..$ id   : chr "buildingModelMain"
# .. .. .. .. .. .. .. .. ..$ class: chr "shiny-html-output"
# .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. ..$ :List of 3
# .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"Run one clinical trial\""
# .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. ..$ :List of 3
# .. .. .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. .. .. ..$ attribs :List of 2
# .. .. .. .. .. .. .. .. ..$ id   : chr "resultsSim1CTModelMain"
# .. .. .. .. .. .. .. .. ..$ class: chr "shiny-html-output"
# .. .. .. .. .. .. .. ..$ children: list()
# .. .. .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. .. ..$ :List of 3
# .. .. .. .. .. ..$ name    : chr "div"
# .. .. .. .. .. ..$ attribs :List of 1
# .. .. .. .. .. .. ..$ data-display-if: chr "input.viewChoice==\"Do a CT experiment\""
# .. .. .. .. .. ..$ children:List of 1
# .. .. .. .. .. .. ..$ : chr "NOT YET IMPLEMENTED"
# .. .. .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. .. .. ..- attr(*, "class")= chr "shiny.tag"
# .. ..- attr(*, "class")= chr "shiny.tag"
# - attr(*, "class")= chr "shiny.tag"
