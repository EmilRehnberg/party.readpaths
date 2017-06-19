readSplitter <- function(nodeSplit){
  splitPoint <- nodeSplit$splitpoint
  if ("levels" %>% is_in(splitPoint %>% attributes %>% names)) {
    splitPoint %>% attr("levels") %>% .[splitPoint]
  } else {
    splitPoint %>% as.numeric
  }
}

hasWeigths <- function(ct, path, terminalNode, pathNumber){
  ct %>%
    party::nodes(pathNumber %>% magrittr::equals(path %>% length) %>% ifelse(terminalNode, path[pathNumber + 1]) ) %>%
    .[[1]] %>% magrittr::use_series("weights") %>% as.logical %>% which
}

rmDuplicateVariables <- function(filters){
  filters %>%
    rev %>%
    data.frame( varName = gsub("^(\\w+).*$", "\\1", .)
               ,filter = .) %>%
    dplyr::group_by(varName) %>%
    dplyr::mutate(rn = row_number()) %>%
    dplyr::filter(rn == 1) %>%
    magrittr::use_series("filter") %>%
    as.character
}

dataFilter <- function(ct, dts, path, terminalNode, pathNumber){
  whichWeights <- hasWeigths(ct, path, terminalNode, pathNumber)
  party::nodes(ct, path[pathNumber])[[1]][[5]] %>%
    buildDataFilter(dts, whichWeights)
}

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
