library(party.readpaths)
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

  it("includes expected terminal nodes", {
    node3path <- "Wind <= 6.9 & Temp <= 82"
    paths %>%
      dplyr::filter(Node == 3) %>%
      use_series(Path) %>%
      as.character %>%
      expect_equal(node3path)

    node6path <- "Temp > 77 & Wind > 6.9"
    paths %>%
      dplyr::filter(Node == 6) %>%
      use_series(Path) %>%
      as.character %>%
      expect_equal(node6path)

    node8path <- "Wind <= 10.3 & Temp > 82"
    paths %>%
      dplyr::filter(Node == 8) %>%
      use_series(Path) %>%
      as.character %>%
      expect_equal(node8path)
  })

  it("order by terminal node number", {
    terminalNodes <- c(3, 5, 6, 8, 9)
    expect_equal(paths$Node, terminalNodes)
  })
})
