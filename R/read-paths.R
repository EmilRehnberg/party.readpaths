#' extract the terminal node paths from a conditional inference tree
#'
#' @export
#' @param ct A ctree.
#' @param dts A data.frame with the data that was the base for the ctree
#' @return A data.frame with number of the terminal node and it's path
#' @examples
#' airq <- subset(airquality, !is.na(Ozone))
#' act <- party::ctree(Ozone ~ .,data = airq)
#' readCtreePaths(act, airq)
readCtreePaths <- function (ct, dts) {
  if (ct %>% treeIsEmpty) return(data.frame())
  ct %>%
    readSegments %>%
    readSplittingCriteria(ct, dts)
}
