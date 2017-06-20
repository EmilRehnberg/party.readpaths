
## party.readpaths R package

The original `party` package is missing a function for reading the splitting
criteria for each terminal node.

This package adds such a function: `readCtreePaths`.

Note: there is successor package to `party` named `partykit`. That package has
a hidden function for the same purpose as `readCtreePaths` that can be found
with `partykit:::.list.rules.party`.

### Example

```r
airq <- subset(airquality, !is.na(Ozone))
act <- party::ctree(Ozone ~ .,data = airq)
readCtreePaths(act, airq)
#>   Node                     Path
#> 1    3 Wind <= 6.9 & Temp <= 82
#> 2    5  Temp <= 77 & Wind > 6.9
#> 3    6   Temp > 77 & Wind > 6.9
#> 4    8 Wind <= 10.3 & Temp > 82
#> 5    9  Wind > 10.3 & Temp > 82
```

See `DESCRIPTION` for dependencies.

