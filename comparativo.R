library(AppliedPredictiveModeling);library(caret)
data(solubility)
trainingData <- solTrainXtrans
trainingData$Solubility <- solTrainY
# Run OLS
ctrl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,set.seed(100))
ols <- train(Solubility~.,data=trainingData,method = "lm", trControl = ctrl)
ols

# Run principal component regression
pcr<- train(Solubility~.,data=trainingData,method = "rlm",preProcess = "pca",trControl = ctrl)
pcr

# Run partial least squares
pls<- train(Solubility~.,data=trainingData,method ="pls", tuneLength = 20,trControl = ctrl,preProc = c("center", "scale"))
pls

plot(pls)

# Compare OLS, PCR, PLS: 
ols$results

pcr$results

pls$results
huyen1=ols$resample
huyen2=pcr$resample
huyen3=pls$resample
huyen1$Mohinh=rep("OLS",100)
huyen2$Mohinh=rep("PCR",100)
huyen3$Mohinh=rep("PLS",100)
huyen=rbind(huyen1[,-3],huyen2[,-3],huyen3[,-3])
huyen$Mohinh<- factor(huyen$Mohinh,levels = c("OLS", "PCR","PLS"),labels = c("OLS", "PCR","PLS"))

library(ggplot2)
ggplot(huyen,aes(Mohinh,RMSE,colour=Mohinh))+geom_boxplot()

ggplot(huyen,aes(Mohinh,Rsquared,colour=Mohinh))+geom_boxplot()

ggplot(huyen, aes(RMSE, fill=Mohinh)) + geom_density(alpha=0.3)

ggplot(huyen, aes(Rsquared, fill=Mohinh)) + geom_density(alpha=0.3)

# t Test for differences:
t.test(huyen1$RMSE,huyen2$RMSE)

t.test(huyen1$Rsquared,huyen3$Rsquared)

t.test(huyen1$RMSE,huyen3$RMSE)
