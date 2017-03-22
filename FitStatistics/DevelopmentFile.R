
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("/Users/hyunjoooh/Desktop/2017_Stat_Prog/PS5")
## This is run once when the package strcuture is first created


## At this point put the *.R files into the correcto directories and edit the DESCRIPTION file

# Now the NAMESPACE

## This can be run many times as the code is updates
current.code <- as.package("FitStatistics")
load_all(current.code)
document(current.code)
test(current.code)

library(FitStatisticsPack)
