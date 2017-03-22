#' Calculating fit statistics
#'
#' Evaluate the fit of various statistical models
#'
#' @param y A numeric vector with observations
#' @param p A matrix of predicted values
#' @param r A numeric vector of fit statistics: RMSE, MAD, RMSLE, MAPE, MEAPE
#'
#' @return A matrix of fit statistic results of each model
#' @author Hyunjoo Oh<\email{hyunjoo.oh@@wustl.edu}>
#' @note Fit statistics include RMSE(root mean squared error), 
#' MAD(median absolute deviation), MEAPE(median absolute percentage error), RMSLE, and MAPE
#' @examples
#' 
#' myX <- rnorm(100, 0, 1) 
#' myY <- sample(size = 100, seq(1:1000))
#' p <- matrix(predict(lm(myY ~ myX), ncol=1))
#' r <- sample(min(myY):max(myY), 100, replace = TRUE)
#' FitStatistics(y=myY, p=p, r=r, fits=c("rmse", "mad", "meape"))
#' @rdname FitStatistics
#' @aliases FitStatistics,ANY-method
#' @export
#' 
FitStatistics <- function(y, p, r = NULL, fits){
  error <- apply(p, 2, function(p) abs(p-y)) # error term = p-y
  e <- apply(error, 2, function(error) na.omit(error)) # remove NAs
  y[y==0 & !is.na(y)] <- y[y==0 & !is.na(y)] + 0.0001 # add 0.0001 in order to prevent the case of y=0
  a <- apply(error, 2, function(error) (error/abs(y)) * 100) # the absolute percentage error
  a <- apply(a, 2, function(a) na.omit(a)) # remove NAs
  n <- nrow(e) # number of rows in error
  output <- NULL # create output
  ## calculate the fitness statistics
  # 1) RMSE(root mean squared error):
  if("rmse" %in% fits){
    RMSE <- apply(e, 2, function(e) sqrt(sum(e^2)/n))
    output <- cbind(output, RMSE)
  }
  # 2) MAD(median absolute deviation):
  if("mad" %in% fits){
    MAD <- apply(e, 2, function(x) median(e))
    output <- cbind(output, MAD)
  }
  # 3) RMSLE
  if("rmsle" %in% fits){
    p.new <- apply(p, 2, function(p) na.omit(p)) # new p without Nas
    y.new <- as.matrix(y[!is.na(p[ ,1])]) # new y for new p
    for(i in 1:nrow(p.new)){
      if (min(p.new[i, ]) < 0 | y.new[i, ] < 0){
        Y <- as.matrix(y.new[-i, ]) # create Y 
        P <- p.new[-i, ] # create P 
      } else {
        Y <- as.matrix(y.new)
        P <- p.new
      }
    }
    RMSLE <- apply(P, 2, function(P) sqrt(sum((log(P+1)-log(Y+1))^2)/n))
    output <- cbind(output, RMSLE)
  }
  # 4) MAPE
  if("mape" %in% fits){
    MAPE <- apply(a, 2, function(a) sum(a)/n)
    output <- cbind(output, MAPE)
  }
  # MEAPE(median absolute percentage error):
  if("meape" %in% fits){
    MEAPE <- apply(a, 2, function(a) median(a))
    output <- cbind(output, MEAPE)
  }
  # MRAE
  if("mrae" %in% fits){
    if(is.null(r)){
      warning("r must be specified for MRAE calculation.")
    } else {
      b <- as.matrix(abs(r-y)) # find b
      b <- as.matrix(b[!is.na(error[ ,1])])
      b[b==0 & !is.na(b)] <- b[b==0 & !is.na(b)]+0.0001 # prevent the case where b=0  
      MRAE <- apply(e, 2, function(e) median(e/b)) # calculate the fit
      output <- cbind(output, MRAE)}}
  print(output)
}