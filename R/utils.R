keepFirstOccuranceForCol <- function(dts, column){
  dts %>%
    dplyrGroupByString(column) %>%
    dplyrWhere(row_number() == 1)
}
