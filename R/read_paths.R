#' extract the terminal node paths from a conditional inference tree
#'
#' @export
#' @param ct A ctree.
#' @param dts A data.frame with the data that was the base for the ctree
#' @return A data.frame with number of the terminal node and it's path
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' act <- party::ctree(Ozone ~ .,data = airq)
#' readTerminalNodePaths(act, airq)
readTerminalNodePaths <- function (ct, dts) {
  if (ct@tree$left %>% identical(NULL)) return(data.frame())
  sgmnts <- ct %>% party::where %>% unique

  # Take the inner nodes smaller than the selected terminal node
  pathForTerminalNode <- function(terminalNode){
    readInnerNodes(sgmnts, terminalNode) %>%
      sapply(function(innerNode){
        if (any(readNodeWeights(ct, terminalNode) &
                nodesFirstTreeWeightIsOne(ct, innerNode))) innerNode
       }) %>%
      unlist
  }

  # Find the splits criteria
  sgmnts %>% sapply(function(terminalNode){
    path <- terminalNode %>% pathForTerminalNode

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
