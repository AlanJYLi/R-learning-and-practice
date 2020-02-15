# Model selection

# import data
library(faraway)
data(debt)
fullData=na.omit(debt)
fullData$incomegp = as.factor(fullData$incomegp)
fullData$house = as.factor(fullData$house)
fullData$singpar = as.factor(fullData$singpar)
fullData$agegp = as.factor(fullData$agegp)
fullData$bankacc = as.factor(fullData$bankacc)
fullData$bsocacc = as.factor(fullData$bsocacc)
fullData$ccarduse = as.factor(fullData$ccarduse)
fullData$cigbuy = as.factor(fullData$cigbuy)
fullData$xmasbuy = as.factor(fullData$xmasbuy)
set.seed(190)
testRows=sample(nrow(fullData),0.1*nrow(fullData))
testData=fullData[testRows, ]
trainData=fullData[-testRows, ]

# full model
model_lm = lm(prodebt ~ ., data=trainData)
summary(model_lm)

library(caret)
# 10-fold CV
set.seed(190)
tenfold = trainControl(method="cv", number=10)
train(prodebt ~ ., data=trainData, method="lm", trControl=tenfold)

# leave-one-out CV
set.seed(190)
leaveoneout = trainControl(method="LOOCV")
train(prodebt ~ ., data=trainData, method="lm", trControl=leaveoneout)

# Mallow's Cp, AIC, BIC
library(CombMSC)
n = nrow(trainData)
results = data.frame()
results[1,1] = Cp(model_lm,S2=(summary(model_lm)$sigma)**2)[1,1]
results[1,2] = AIC(model_lm,k=2)
results[1,3] = AIC(model_lm,k=log(n))
colnames(results) = c("Mallow's Cp", "AIC", "BIC")
results


# Full search
library(faraway)
data(debt)
fullData_dummy=na.omit(debt)
fullData_dummy$incomegp2 = 0
fullData_dummy$incomegp3 = 0
fullData_dummy$incomegp4 = 0
fullData_dummy$incomegp5 = 0
fullData_dummy[fullData_dummy$incomegp==2, "incomegp2"] = 1
fullData_dummy[fullData_dummy$incomegp==3, "incomegp3"] = 1
fullData_dummy[fullData_dummy$incomegp==4, "incomegp4"] = 1
fullData_dummy[fullData_dummy$incomegp==5, "incomegp5"] = 1
fullData_dummy$house2 = 0
fullData_dummy$house3 = 0
fullData_dummy[fullData_dummy$house==2, "house2"] = 1
fullData_dummy[fullData_dummy$house==3, "house3"] = 1
fullData_dummy$agegp2 = 0
fullData_dummy$agegp3 = 0
fullData_dummy$agegp4 = 0
fullData_dummy[fullData_dummy$agegp==2, "agegp2"] = 1
fullData_dummy[fullData_dummy$agegp==3, "agegp3"] = 1
fullData_dummy[fullData_dummy$agegp==4, "agegp4"] = 1
fullData_dummy$ccarduse2 = 0
fullData_dummy$ccarduse3 = 0
fullData_dummy[fullData_dummy$ccarduse==2, "ccarduse2"] = 1
fullData_dummy[fullData_dummy$ccarduse==3, "ccarduse3"] = 1
fullData_dummy = fullData_dummy[,-c(1,2,5,9)]
set.seed(190)
testRows=sample(nrow(fullData_dummy),0.1*nrow(fullData_dummy))
testData_dummy=fullData_dummy[testRows, ]
trainData_dummy=fullData_dummy[-testRows, ]

library(leaps)
full_search = leaps(x=trainData_dummy[,-9], y=trainData_dummy[,9],
                    names=colnames(trainData_dummy[,-9]), method="Cp")
write.table(cbind(as.matrix(full_search$which),full_search$Cp),
            file="full search.csv", sep=",", row.names=FALSE)

best.model = which(full_search$Cp==min(full_search$Cp))
cbind(as.matrix(full_search$which),full_search$Cp)[best.model,]


# Stepwise method
start = lm(prodebt ~ 1, data=trainData)
full = lm(prodebt ~ ., data=trainData)
forward_step = step(start, scope=list(lower=start,upper=full),direction="forward")
summary(forward_step)

start = lm(prodebt ~ 1, data=trainData)
full = lm(prodebt ~ ., data=trainData)
backward_step = step(full, scope=list(lower=start,upper=full),direction="backward")
summary(backward_step)

compare = data.frame()
compare[1,1] = summary(model_lm)$adj.r.squared
compare[2,1] = Cp(model_lm,S2=(summary(model_lm)$sigma)**2)[1,1]
compare[3,1] = AIC(model_lm,k=2)
model_q2 = lm(prodebt~bankacc+manage+xmasbuy+locintrn+incomegp
              +agegp+ccarduse, data=trainData)
compare[1,2] = summary(model_q2)$adj.r.squared
compare[2,2] = Cp(model_q2,S2=(summary(model_lm)$sigma)**2)[1,1]
compare[3,2] = AIC(model_q2,k=2)
model_fd = lm(prodebt~bankacc+manage+xmasbuy+locintrn+ccarduse
              +agegp, data=trainData)
compare[1,3] = summary(model_fd)$adj.r.squared
compare[2,3] = Cp(model_fd,S2=(summary(model_lm)$sigma)**2)[1,1]
compare[3,3] = AIC(model_fd,k=2)
colnames(compare) = c("Full Model", "Q2 Model", "Forward Selection Model")
rownames(compare) = c("adjested R2", "Mallow's Cp", "AIC")
compare


# Ridge regression
library(MASS)
# Scale the numeric predicting variables and the response variable
trainData_scale = trainData_dummy
trainData_scale$children = scale(trainData_scale$children)
trainData_scale$manage = scale(trainData_scale$manage)
trainData_scale$locintrn = scale(trainData_scale$locintrn)
trainData_scale$prodebt = scale(trainData_scale$prodebt)
# Apply ridge regression for a range of penalty constants
lambda = seq(0, 200, by=5)
ridge_reg = lm.ridge(prodebt~., data=trainData_dummy, lambda=lambda)   
round(ridge_reg$GCV, 7)

lambda = seq(70, 85, by=1)
ridge_reg = lm.ridge(prodebt~., data=trainData_scale, lambda=lambda)   
round(ridge_reg$GCV, 7)
which(ridge_reg$GCV == min(ridge_reg$GCV))

ridge_reg$coef[,9]


# Lasso regression
library(glmnet)
set.seed(190)
lasso_cv = cv.glmnet(x=as.matrix(trainData_scale[,-9]), 
                     y=as.matrix(trainData_scale[,9]), 
                     alpha=1, nfolds=10)
lasso_cv$lambda.min

lasso_model = glmnet(x=as.matrix(trainData_scale[,-9]), 
                     y=as.matrix(trainData_scale[,9]), 
                     alpha=1, nlambda=100)
plot(lasso_model, xvar="lambda")
abline(v=log(lasso_cv$lambda.min), col='black', lty =2)

coef(lasso_model,s=lasso_cv$lambda.min)


# Elastic Net
set.seed(190)
elastic_cv = cv.glmnet(x=as.matrix(trainData_scale[,-9]), 
                       y=as.matrix(trainData_scale[,9]), 
                       alpha=0.5, nfolds=10)
elastic_cv$lambda.min

elastic_model = glmnet(x=as.matrix(trainData_scale[,-9]), 
                       y=as.matrix(trainData_scale[,9]), 
                       alpha=0.5, nlambda=100)
coef(elastic_model,s=elastic_cv$lambda.min)


# Model comparison
model_full = lm(prodebt ~ ., data=trainData)
model_lowcp = lm(prodebt~bankacc+manage+xmasbuy+locintrn+incomegp
                 +agegp+ccarduse, data=trainData)
model_forward = lm(prodebt~bankacc+manage+xmasbuy+locintrn+ccarduse
                   +agegp, data=trainData)
model_lasso = lm(prodebt~children+bankacc+bsocacc+manage+cigbuy
                 +xmasbuy+locintrn+incomegp+house+agegp+ccarduse, 
                 data=trainData)
model_elastic = lm(prodebt~children+bankacc+bsocacc+manage+cigbuy
                   +xmasbuy+locintrn+incomegp+house+agegp+ccarduse, 
                   data=trainData)

model_full_predict = predict(model_full,testData[,-13])
model_lowcp_predict = predict(model_lowcp,testData[,-13])
model_forward_predict = predict(model_forward,testData[,-13])
model_lasso_predict = predict(model_lasso,testData[,-13])
model_elastic_predict = predict(model_elastic,testData[,-13])

testData_scale = testData_dummy

m = attributes(trainData_scale$children)$'scaled:center'
sd = attributes(trainData_scale$children)$'scaled:scale'
testData_scale$children = (testData_scale$children-m)/sd

m = attributes(trainData_scale$manage)$'scaled:center'
sd = attributes(trainData_scale$manage)$'scaled:scale'
testData_scale$manage = (testData_scale$manage-m)/sd

m = attributes(trainData_scale$locintrn)$'scaled:center'
sd = attributes(trainData_scale$locintrn)$'scaled:scale'
testData_scale$locintrn = (testData_scale$locintrn-m)/sd

model_ridge = lm.ridge(prodebt~., data=trainData_scale, lambda=78)

y_pred_scale = as.matrix(testData_scale[,-9]) %*% model_ridge$coef + model_ridge$ym

m = attributes(trainData_scale$prodebt)$'scaled:center'
sd = attributes(trainData_scale$prodebt)$'scaled:scale'
model_ridge_predict = y_pred_scale*sd+m

compare = data.frame()
compare[1,1] = mean((model_full_predict-testData[,13])^2)
compare[2,1] = mean((model_lowcp_predict-testData[,13])^2)
compare[3,1] = mean((model_forward_predict-testData[,13])^2)
compare[4,1] = mean((model_lasso_predict-testData[,13])^2)
compare[5,1] = mean((model_elastic_predict-testData[,13])^2)
compare[6,1] = mean((model_ridge_predict-testData[,13])^2)
rownames(compare) = c("model_full", "model_lowcp", "model_forward", 
                      "model_lasso", "model_elastic", "model_ridge")
colnames(compare) = c("MSE")
compare


# list the features selected by different method
table = data.frame()
table[seq(1,12),1] = c(1,0,0,0,1,1,0,1,1,0,1,1)
table[seq(1,12),2] = c(0,0,0,0,1,1,0,1,1,0,1,1)
table[seq(1,12),3] = c(1,1,1,0,1,1,1,1,1,1,1,1)
table[seq(1,12),4] = c(1,1,1,0,1,1,1,1,1,1,1,1)
colnames(table) = c("Best subset&Mallow's Cp", "Stepwise&AIC", 
                    "Lasso&10-foldCV", "Elastic&10-foldCV")
rownames(table) = colnames(trainData[,-13])
table