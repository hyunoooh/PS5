pkgname <- "FitStatisticsPack"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('FitStatisticsPack')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("FitStatistics")
### * FitStatistics

flush(stderr()); flush(stdout())

### Name: FitStatistics
### Title: Calculating fit statistics
### Aliases: FitStatistics FitStatistics,ANY-method

### ** Examples


myX <- rnorm(100, 0, 1) 
myY <- sample(size = 100, seq(1:1000))
p <- matrix(predict(lm(myY ~ myX), ncol=1))
r <- sample(min(myY):max(myY), 100, replace = TRUE)
FitStatistics(y=myY, p=p, r=r, fits=c("rmse", "mad", "meape"))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
