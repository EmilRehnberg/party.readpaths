buildDataFilter <- function(nodeSplit, ...) UseMethod("buildDataFilter")

buildDataFilter.nominalSplit <-
  function(nodeSplit, dts, whichWeights){
    buildNominalCriteria( nodeSplit$variableName
                         ,dts[ whichWeights, nodeSplit$variableName] %>% unique)
  }

buildDataFilter.orderedSplit <-
  function(nodeSplit, dts, whichWeights){
    buildOrdinalCriteria( nodeSplit$variableName
                         ,nodeSplit %>% readSplitter
                         ,dts[whichWeights,])
}

readSplitter <- function(nodeSplit){
  splitPoint <- nodeSplit$splitpoint
  if ("levels" %>% is_in(splitPoint %>% attributes %>% names)) {
    splitPoint %>% attr("levels") %>% .[splitPoint]
  } else {
    splitPoint %>% as.numeric
  }
}

buildNominalCriteria <- function(varName, inclLevels){
  paste( varName
        ,"=="
        ,inclLevels %>% paste(collapse = ", ") %>% paste0("{", ., "}"))
}

buildOrdinalCriteria <- function(varName, splitter, dts){
  dts[,varName] %>%
    is_weakly_less_than(splitter) %>%
    all %>%
    ifelse("<=", ">") %>%
    paste(varName, ., splitter)
}
