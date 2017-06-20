describe("readCtreePaths()", {
  airq <- subset(airquality, !is.na(Ozone))
  act <- party::ctree(Ozone ~ .,data = airq)
  paths <- readCtreePaths(act, airq)

  it("returns a data.frame", {
    expect_is(paths, "data.frame")
  })

  it("returns empty data.frame for empty trees", {
    party::ctree(Day ~ ., data = airquality) %>%
      readCtreePaths(airquality) %>%
      expect_equal(data.frame())
  })

  pathForNode <- function(paths, nodeNumber){
    paths %>%
      dplyr::filter(Node == nodeNumber) %>%
      use_series(Path) %>%
      as.character
  }

  it("includes expected terminal nodes", {
    expect_equal(pathForNode(paths, 3), "Wind <= 6.9 & Temp <= 82")
    expect_equal(pathForNode(paths, 6), "Temp > 77 & Wind > 6.9")
    expect_equal(pathForNode(paths, 8), "Wind <= 10.3 & Temp > 82")
  })

  it("works as expected with mixed ordinal and nominal variables", {
    load("./lead-buyrate.RData") # loads gdt
    gct <- ctree( is_buyer ~ ., data = gdt)
    mixedPaths <- readCtreePaths(gct, gdt)

    expect_equal(pathForNode(mixedPaths, 4), "city == {LA}")
    expect_equal(pathForNode(mixedPaths, 8), "point > 0.851221123244613 & age <= mid & city == {Chigaco, Memphis, Boston}")
  })

  it("order by terminal node number", {
    terminalNodes <- c(3, 5, 6, 8, 9)
    expect_equal(paths$Node, terminalNodes)
  })
})
