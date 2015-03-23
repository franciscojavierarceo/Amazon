n <- 1e3
x1 <- rnorm(n,mean=0);x1 <- scale(x1)
x2 <- rnorm(n,mean=0);x2 <- scale(x2)
e  <- rnorm(n); e <- scale(e)
yhat <- 2*x1 + 1*x2 
yhat <- yhat - mean(yhat)
ys <- yhat + e
summary(lm(y~yhat))$r.square
xs <- model.matrix(~x1+x2-1)
olsmod <- glmnet(x=xs,
                y=ys,
                alpha=0,
                lambda=0.0,
                intercept=FALSE)
l1mod <- glmnet(x=xs,
              y=ys,
              alpha=1,
              lambda=0.25,
              intercept=FALSE)
l2mod <- glmnet(x=xs,
                y=ys,
                alpha=0,
                lambda=0.25,
                intercept=FALSE)
coef(olsmod)-0.25
print('Lasso Coefficients:')
coef(l1mod)
coef(olsmod)/1.25
print("Ridge Coefficients:")
coef(l2mod)
solve(t(xs)%*%xs + diag(dim(xs)[2])*0.25)%*%t(xs)%*%ys
solve(t(xs)%*%xs + diag(dim(xs)[2])*0.00)%*%t(xs)%*%ys
