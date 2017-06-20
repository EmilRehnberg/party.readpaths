buildDataFilter <- function(nodeSplit, ...) UseMethod("buildDataFilter")

buildDataFilter.nominalSplit <-
  function(nodeSplit, dts, whichWeights){
    varName <- nodeSplit$variableName
    includedLevels <- dts[ whichWeights
                          ,varName] %>% unique
    paste( varName, "=="
          ,includedLevels %>% paste(collapse = ", ") %>% paste0("{", ., "}"))
  }

buildDataFilter.orderedSplit <-
  function(nodeSplit, dts, whichWeights){
    varName <- nodeSplit$variableName
    splitter <- nodeSplit %>% readSplitter

    dts[ whichWeights
        ,varName] %>%
          is_weakly_less_than(splitter) %>%
          all %>%
          ifelse("<=", ">") %>%
          paste(varName, ., splitter)
}
