#===========================================================================================
# Importing required packages
# Data from https://www.kaggle.com/c/amazon-employee-access-challenge/data
#===========================================================================================
require(ggplot2)
require(scales)
require(Matrix)
require(glmnet)
require(sqldf)
require(reshape2)
require(gridExtra)
require(foreach)
require(parallel)
require(doParallel)
require(ROCR)
#===========================================================================================
setwd('~/GitHub/Regularization/')
#===========================================================================================
df1 <- read.csv('train.csv')
head(df1)
ydep <- df1$ACTION
n <- dim(df1)[1]
#===========================================================================================
set.seed(40);trainfilt <- ifelse(runif(n)<=0.7,1,0)            # Random Training split
validfilt <- trainfilt==0
#===========================================================================================
for(i in 2:dim(df1)[2]){
  df1[,i] <- factor(df1[,i])
}
#===========================================================================================
xs <- sparse.model.matrix(~.,data=df1[,2:10])
print(dim(xs))
detectCores(all.tests=TRUE)
registerDoParallel(cores=all)
t0 <- Sys.time()

print("...running models")
# Useful just for plotting
ridge_model <- glmnet(x= xs[trainfilt==1,1:60],           
                      y=ydep[trainfilt==1],     
                      alpha  = 0,nlambda=100,
                      family='binomial')

print("ridge finished...")
lasso_model <- glmnet(x= xs[trainfilt==1,1:60],           
                      y=ydep[trainfilt==1],     
                      alpha  = 1,nlambda=100,
                      family='binomial')

print("lasso finished...")
ridge_cvmod <- cv.glmnet(x=xs[trainfilt==1,],     # Specifying sparse input matrix
                         y=ydep[trainfilt==1],    # Specifying dependent variable
                         alpha  = 0,              # Alpha=1 -> Lasso; Alpha=0 -> Ridge
                         type.measure='auc',      # Mean Squared Error
                         nlambda= 50,             # Number of lambda values to try
                         nfolds=10,               # Number of crossfold validations
                         family='binomial')
print("optimal lasso finished...")
lasso_cvmod <- cv.glmnet(x=xs[trainfilt==1,],     # Specifying sparse input matrix
                         y=ydep[trainfilt==1],     # Specifying dependent variable
                         alpha  = 1.0,             # Alpha=1 -> Lasso; Alpha=0 -> Ridge
                         type.measure='auc',       # Mean Squared Error
                         nlambda= 50,              # Number of lambda values to try
                         nfolds=10,                # Number of crossfold validations
                         family='binomial')
tEnd <- Sys.time()
print("The running time of GLMNET was:")
print(tEnd-t0)
#===========================================================================================
pdf('RidgeModel1.pdf',width=7,height=7)
plot(ridge_model,xlab='L2 Norm - Ridge Coefficients')
dev.off()
pdf('RidgeModel2.pdf',width=7,height=7)
plot(ridge_cvmod,xlab='AUC as a function of log(Lambda) \n Ridge')
dev.off()
pdf('LassoModel1.pdf',width=7,height=7)
plot(lasso_model,xlab='L1 Norm - LASSO Coefficients')
dev.off()
pdf('LassoModel2.pdf',width=7,height=7)
plot(lasso_cvmod,xlab='AUC as a function of log(Lambda) \n LASSO',ylim=c(0.5,1))
dev.off()
#===========================================================================================
ylasso <- predict(ridge_cvmod,newx=xs,s='lambda.min')
yridge <- predict(lasso_cvmod,newx=xs,s='lambda.min')
yolsmd <- predict(ridge_cvmod,newx=xs,s=min(ridge_cvmod$lambda)) # This is closest to OLS
#===========================================================================================
v1 <- round(auc(ydep[validfilt],yolsmd[validfilt]),4)
v2 <- round(auc(ydep[validfilt],yridge[validfilt]),4)
v3 <- round(auc(ydep[validfilt],ylasso[validfilt]),4)
#===========================================================================================
md1 <- prediction( yolsmd[validfilt],ydep[validfilt])
md2 <- prediction( yridge[validfilt],ydep[validfilt])
md3 <- prediction( ylasso[validfilt],ydep[validfilt])
md4 <- prediction( ydep[validfilt],ydep[validfilt])

prf1 <- performance( md1, "tpr", "fpr" )
prf2 <- performance( md2, "tpr", "fpr" )
prf3 <- performance( md3, "tpr", "fpr" )
prf4 <- performance( md4, "tpr", "fpr" )

lbls <- paste(c('OLS','Ridge','LASSO','Random','Perfect'),' AUC=',c(v1,v2,v3,0.5,1),sep=' ')
par(mfrow=c(1,1))

jpeg('AUCbyModel.jpeg', width=960,height=960)
plot(prf1, colorize = FALSE,col='deepskyblue',main='Model Performance by Regression Method \n (Hold Out)')
plot(prf2, add = TRUE, colorize = FALSE,col='red')
plot(prf3, add = TRUE, colorize = FALSE,col='green')
plot(prf4, add = TRUE, colorize = FALSE,col='purple3')
grid(5,5,'gray44');abline(0,1,col='black')
legend('bottomright',lbls,col=c('deepskyblue','red','green','purple3'),lty=c(1,1,1))
dev.off()

smrdf <- data.frame(Val=c(0.5,v1,v2,v3,1),Type=c('Random','OLS','Ridge','LASSO','Perfect'))
smrdf$Type <- factor(smrdf$Type,levels=smrdf$Type[order(smrdf$Val,decreasing=T)])
pdf('ModelPerformance.pdf',width=7,height=7)
ggplot(smrdf,aes(x=Type,y=Val))+geom_bar(stat='identity',fill='darkblue')+
  geom_text(aes(label=Val,vjust=-0.5))+xlab('')+ylab('AUC')+ylim(0,1)+
  theme_bw()+ggtitle('AUC by Model Type')
dev.off()
#===========================================================================================
