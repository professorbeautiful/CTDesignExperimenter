cat("======== fixHtmlEscape.R ================\n")

### to fix a problem where the args text euals character(0).


### 2013-08-06
### See clone at /Users/Roger/Dropbox/_HOME/R-in-Dropbox/shiny/R
### the output of my investigations using sys.frames() etc. is at 
### /Users/Roger/Dropbox/_HOME/CT-design-simulator---normolle,yuanyuan/fixHtmlEscape.R

### htmlEscape was moved from shiny to htmltools.
assignInNamespace(ns="htmltools", x="htmlEscape", 
                  function(text, attribute=TRUE, browse=TRUE) {
  .htmlSpecials <- list(
    `&` = '&amp;',
    `<` = '&lt;',
    `>` = '&gt;'
  )
  .htmlSpecialsPattern <- paste(names(.htmlSpecials), collapse='|')
  .htmlSpecialsAttrib <- c(
    .htmlSpecials,
    `'` = '&#39;',
    `"` = '&quot;',
    `\r` = '&#13;',
    `\n` = '&#10;'
  )
  .htmlSpecialsPatternAttrib <- paste(names(.htmlSpecialsAttrib), collapse='|')
  
    pattern <- if(attribute)
      .htmlSpecialsPatternAttrib 
    else
      .htmlSpecialsPattern
  if(length(text)==0){
    if(browse) browser()
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
