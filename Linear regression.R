# import data
dataFull = read.csv("abalone.csv",head=T)
dataTest = dataFull[(nrow(dataFull)-10):nrow(dataFull),]
data = dataFull[1:(nrow(dataFull)-11),]

# boxplot of the qualitative predictor and the response
boxplot(data$Rings~data$Sex, main='Sex')
summary(aov(data$Rings~data$Sex))
TukeyHSD(aov(data$Rings~data$Sex), conf.level = 0.99)

# scatterplot matrix containing the quantitative predictors and the response
library(graphics)
drawplot = function(x,y) {
  points(x,y)
  abline(lm(y~x), col='red')
  r = round(cor(x, y), digits=2)
  txt = paste('r=',r)
  par(usr = c(0, 0.6, 0.3, 6))
  text(0.5, 0.9, txt, col='red')
}
pairs(data[,-1], panel = drawplot, main = 'Pairwise Scatterplot')

# fit the multiple linear regression model
model1 = lm(Rings~., data = data)
summary(model1)
confint(model1, 'Length', level = 0.9)

# check assumption
stdres = rstandard(model1)
plot(y = stdres, x = model1$fitted.values, 
     xlab = 'Fitted Value', ylab = 'Standardized Residuals',
     main = 'Residuals Analysis')
abline(a = -1.5, b = 0.5, col = 'red')
abline(a = -1, b = -0.1, col = 'red')
abline(h = 0, col = 'blue')

res = model1$residuals
hist(res, breaks = 40, main="Histogram of residuals", xlab="Residuals")
qqnorm(res, 
       ylab="Residuals", 
       xlab="Normal Scores", 
       main="QQ Plot of Residuals") 
qqline(res)

cook = cooks.distance(model1)
cook[cook>1]
plot(cook,type = 'h', lwd = 1, ylab = "Cook's Distance", main = "Cook's Distance")
abline(h=1, col='red')
cook2 = cook[-2052]
plot(cook2,type = 'h', lwd = 1, ylab = "Cook's Distance"
     , main = "Cook's Distance without 2052rd Data Point")
abline(h=4/4166, col='red')

library(car)
cat('VIF Threshold',max(10,1/(1-summary(model1)$r.squared)),'\n')
vif(model1)


# model comparison
model2 = lm(Rings~Sex + Diameter + Height + Shucked + Shell, data = data)
summary(model2)

compare_r = data.frame()
compare_r[1,1] = 'model1'
compare_r[1,2] = round(summary(model1)$r.squared, 4)
compare_r[1,3] = round(summary(model1)$adj.r.squared, 4)
compare_r[2,1] = 'model2'
compare_r[2,2] = round(summary(model2)$r.squared, 4)
compare_r[2,3] = round(summary(model2)$adj.r.squared, 4)
colnames(compare_r) = c('model', 'R squared', 'Adj. R squared')
compare_r

anova(model2, model1)

newdata = dataTest[,-9]
model1_predict = predict(model1,newdata,interval=c("prediction"))
cat('Model1:\n');model1_predict
model2_predict = predict(model2,newdata,interval=c("prediction"))
cat('\nModel2:\n');model2_predict
interval_range = data.frame('model1 interval range'=model1_predict[,3]-model1_predict[,2],
                            'model2 interval range'=model2_predict[,3]-model2_predict[,2])
cat('Compare interval range\n');interval_range

paste('MSPE of model1:',mean((model1_predict[,1]-dataTest[,9])^2))
paste('MSPE of model2:',mean((model2_predict[,1]-dataTest[,9])^2))
paste('PM of model1:',
      sum((model1_predict[,1]-dataTest[,9])^2)/sum((dataTest[,9]-mean(dataTest[,9]))^2))
paste('PM of model2:',
      sum((model2_predict[,1]-dataTest[,9])^2)/sum((dataTest[,9]-mean(dataTest[,9]))^2))


new = data.frame('Sex'='F','Diameter'=0.4,'Height'=0.2,'Shucked'=0.3,'Shell'=0.3)
new_abalone = predict(model2, new, interval="predict", level=0.9)
new_abalone
paste('age is ', new_abalone[,1]*1.5)
