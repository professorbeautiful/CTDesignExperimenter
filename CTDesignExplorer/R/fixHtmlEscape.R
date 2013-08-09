
assignInNamespace(ns="shiny", x="htmlEscape", 
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
