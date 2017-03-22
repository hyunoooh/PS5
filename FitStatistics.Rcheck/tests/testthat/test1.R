context("Calculating fit statistics of statistical models")
expect_is(FitStatistics(y = sample(size = 100, seq(1:10000)),
                        p = matrix(rnorm(100, 50, 10), ncol=1), 
                        r = sample(min(y):max(y), 100, replace = TRUE),
                        fits = c("rmse", "mad", "mape", "meape", "rmsle", "mrae")), "matrix"
)