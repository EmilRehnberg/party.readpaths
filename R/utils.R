keepFirstOccuranceForCol <- function(dts, column){
  dts %>%
    dplyr::group_by_(column) %>%
    dplyr::filter(row_number() == 1)
}
