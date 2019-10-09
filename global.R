## Load Require Packages
p <- c("shiny", "plotly", "data.table")
sapply(p, function(x) require(x,char=T))

## Load Data
cities <- fread("cities.csv", stringsAsFactors = F)
#names(cities)[1] <- "Rank"