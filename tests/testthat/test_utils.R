describe("keepFirstOccuranceForCol()", {
  firstOccuranceData <-
    data.frame( a = c(12, 12, 1, 12, 1, 3, 3, 1)
               ,b = letters[1:8]
               ,stringsAsFactors = FALSE) %>%
      keepFirstOccuranceForCol("a")

  it("returns a data.frame", { expect_is(firstOccuranceData, "data.frame") })

  it("returns the observations that have first occurance of the value in column", {
    expect_equal(firstOccuranceData$b, c("a", "c", "f"))
   })

  it("returns the first occurances of the column", {
    expect_equal(firstOccuranceData$a, c(12, 1, 3))
   })
})
