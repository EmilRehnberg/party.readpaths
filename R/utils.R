keepFirstOccuranceForCol <- function(dts, column){
  dts %>%
    dplyrGroupByString(column) %>%
    dplyrWhere(dplyr::row_number() == 1)
}
