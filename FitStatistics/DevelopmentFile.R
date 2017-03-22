
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

## Let's look at a function
getSquares
getMethod(getSquares, "Squares")
getMethod(getSquares, "AllSquares")
getMethod(allSquares)

## Let's try it out
x<-c(1,2)
y<-c(3,4)
allObj <- allSquares(x, y)
allObj
getSquares(allObj)

## Moving between classes
addObj <- addSquares(x,y)
addObj
as(object=addObj, Class="AllSquares")



