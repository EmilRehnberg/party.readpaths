treeIsEmpty <- function(ct) ct@tree$left %>% identical(NULL)
readSegments <- function(ct) ct %>% party::where %>% unique
readNodeWeights <- function(ct, node) party::nodes(ct, node)[[1]]$weights
nodesFirstTreeWeightIsOne <- function(ct, node) party::nodes(ct, node)[[1]][2][[1]] == 1
readInnerNodes <- function(sgmnts, node) setdiff( 1:(node - 1) ,sgmnts[sgmnts < node])

# Take the inner nodes smaller than the selected terminal node
readNodePathForTerminalNodes <- function(terminalNode, sgmnts, ct){
  readInnerNodes(sgmnts, terminalNode) %>%
    sapply(function(innerNode){
      if (any(readNodeWeights(ct, terminalNode) & nodesFirstTreeWeightIsOne(ct, innerNode))) innerNode
     }) %>%
    unlist
}

readSplitter <- function(nodeSplit){
  splitPoint <- nodeSplit$splitpoint
  if ("levels" %>% is_in(splitPoint %>% attributes %>% names)) {
    splitPoint %>% attr("levels") %>% .[splitPoint]
  } else {
    splitPoint %>% as.numeric
  }
}

pathFromPathNumber <- function(path, pathNumber, terminalNode){
  pathNumber %>%
    magrittr::equals(path %>% length) %>%
    ifelse(terminalNode, path[pathNumber + 1])
}

hasWeigths <- function(ct, path, terminalNode, pathNumber){
  ct %>%
    party::nodes(pathFromPathNumber(path, pathNumber, terminalNode)) %>%
    dplyr::first %>%
    magrittr::use_series("weights") %>%
    as.logical %>%
    which
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

readSplittingCriteria <- function(sgmnts, ct, dts){
  sgmnts %>% sapply(function(terminalNode){
    path <- terminalNode %>% readNodePathForTerminalNodes(sgmnts, ct)

    path %>% length %>% seq %>%
      sapply(function(nodeNumber){
        dataFilter(ct, dts, path, terminalNode, nodeNumber)
       }, simplify = FALSE) %>%
      unlist %>% rmDuplicateVariables %>%
      paste(collapse = " & ") %>%
      data.frame(Node = terminalNode, Path = .)

  }, simplify = FALSE) %>%
    Reduce(f = rbind) %>%
    dplyr::arrange(Node)
}
