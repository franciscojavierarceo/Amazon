#==========================================================================
# Author: Francisco Javier Arceo
# Purpose: This is a small and edifying example of L1 and L2 regularization
#   using the glmnet implementatino in R.
# Last Update Date: March 23rd 2015
#==========================================================================
require(glmnet)
#==========================================================================
n <- 1e3 # Setting sample size
x1 <- rnorm(n,mean=0,sd=1);x2 <- rnorm(n,mean=0,sd=1)
x1 <- scale(x1); x2 <- scale(x2)
xstmp <- mvrnorm(n,rep(0,2),matrix(c(1,0,0,1),ncol=2),empirical=T)
x1 <- xstmp[,1]
x2 <- xstmp[,2]
ws <- as.matrix(c(2,1))
e  <- rnorm(n); e <- scale(e)
yhat <- ws[1]*x1 + ws[2]*x2 
yhat <- yhat - mean(yhat)
ys <- yhat + e
vl <- summary(lm(ys~yhat))$r.square
paste("We can explain ",round(vl*100,4),'% of the variance.',sep='')
#==========================================================================
returnweights <- function(xs,ys,lambdaval=0.25){
  olsmod <- glmnet(x=xs,
                   y=ys,
                   alpha=0,
                   lambda=0,
                   intercept=FALSE)
  l1mod <- glmnet(x=xs,
                  y=ys,
                  alpha=1,
                  lambda=lambdaval,
                  intercept=FALSE)
  l2mod <- glmnet(x=xs,
                  y=ys,
                  alpha=0,
                  lambda=lambdaval,
                  intercept=FALSE)
  suppressWarnings(out <- data.frame(OLS=as.matrix(coef(olsmod)),
                    Lasso=data.frame(as.matrix(coef(l1mod))),
                    Ridge=data.frame(as.matrix(coef(l2mod)))))
  names(out) <- c('OLS','Lasso','Ridge')
  out <- round(out,5)
  return(out)
}
xs1 <- as.matrix(cbind(x1,x2),ncol=2)
xs2 <- as.matrix(cbind(x1,x1),ncol=2)
xs3 <- as.matrix(cbind(x1,x1,x1),ncol=3)
#==========================================================================
returnweights(xs1,ys,lambdaval=0.25)
# GLMNET's implementation of Ridge does some adjustment to the design matrix
# so it doesn't exactly return OLS/(1+\lambda)
returnweights(xs1,ys,lambdaval=0.25)$OLS/returnweights(xs1,ys,lambdaval=0.25)$Ridge
# Notice the behavior of the coefficients as we 
#   increase lambda by a small amount -- One variable is kicked out of lasso
returnweights(xs1,ys,lambdaval=1) # But all features stay in Ridge
# Now by a lot--Ridge is tending to zero
returnweights(xs1,ys,lambdaval=1e4)
# Here's a nice example of an interesting question:
#   What happens when you have perfect multicolinearity?
#   i.e., the same variable twice/an undeterimined design matrix
returnweights(xs2,ys,lambdaval=0.25)
# Lasso picks the first one -- this is arbitrary 
# Ridge splits the size of the weight of x1 (previously = 2) equally 
returnweights(xs3,ys,lambdaval=0.25)
# Again, just to emphasize, Ridge splits it equally across all 3 columns 
# that are the same
#==========================================================================
