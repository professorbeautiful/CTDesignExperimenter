cat("=========== ParameterGenerator.R ===========\n")

### A ParameterGenerator as a VariableGenerator 
# that just passes through the parameter into the output variable.

ParameterGenerator = function(varList, varValues, insertSubType, name="no name yet") {
  parameters = varValues
  names(parameters) = names(varList)
  generatorCode = function(){
    parameters
  }
  VariableGenerator(insertSubType=insertSubType, 
                    generatorCode = generatorCode,
                    name=name,
                    parameters = parameters,
                   provisions=varList,
                  )
}
