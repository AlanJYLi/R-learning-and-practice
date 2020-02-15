# GLM: Poisson

# fit the model
library(faraway)
data(gala)
mydata = gala
mydata = mydata[-2]
model1 = glm(Species~., family="poisson", data=mydata)
summary(model1)

confint(model1, 'Nearest', level = 0.9)

# is the model significant?
1-pchisq((3510.73-716.85),(29-24))

# Goodness of fit

# Deviance Test for GOF
with(model1, cbind(res.deviance=deviance, df=df.residual, 
                   p=1-pchisq(deviance, df.residual)))

# GOF test using Pearson residuals
respearson = residuals(model1,type="pearson")
pearson.tvalue = sum(respearson^2)
cbind(pearson.tvalue=pearson.tvalue, df=model1$df.residual, 
      p=1-pchisq(pearson.tvalue, model1$df.residual))

# residual plot
res = resid(model1,type="deviance")
par(mfrow=c(1,2))
qqnorm(res) 
qqline(res, col="red")
hist(res,10,xlab="Deviance Residuals")

specieslog = log(mydata$Species)
par(mfrow=c(2,3))
plot(x=mydata$Area, y=specieslog, 
     ylab="Log of Species", 
     xlab="Area")
plot(x=mydata$Elevation, y=specieslog, 
     ylab="Log of Species", 
     xlab="Elevation")
plot(x=mydata$Nearest, y=specieslog, 
     ylab="Log of Species", 
     xlab="Nearest")
plot(x=mydata$Scruz, y=specieslog, 
     ylab="Log of Species", 
     xlab="Scruz")
plot(x=mydata$Adjacent, y=specieslog, 
     ylab="Log of Species", 
     xlab="Adjacent")


# Fitting a Count per Area Model
model2 = glm(Species~Elevation+Nearest+Scruz+Adjacent+offset(log(Area)), 
             family="poisson", data=mydata)
summary(model2)

library(aod)
wald.test(b=coef(model2), Sigma=vcov(model2), Terms=c(1,3))

# Deviance Test for GOF
with(model2, cbind(res.deviance=deviance, df=df.residual, 
                   p=1-pchisq(deviance, df.residual)))

# GOF test using Pearson residuals
respearson2 = residuals(model2,type="pearson")
pearson.tvalue2 = sum(respearson2^2)
cbind(pearson.tvalue=pearson.tvalue2, df=model2$df.residual, 
      p=1-pchisq(pearson.tvalue2, model2$df.residual))

res2 = resid(model2,type="deviance")
par(mfrow=c(1,2))
qqnorm(res2) 
qqline(res2, col="red")
hist(res2,10,xlab="Deviance Residuals")

densitylog = log(mydata$Species/mydata$Area)
par(mfrow=c(2,2))
plot(x=mydata$Elevation, y=densitylog, 
     ylab="Log of Density", 
     xlab="Elevation")
plot(x=mydata$Nearest, y=densitylog, 
     ylab="Log of Density", 
     xlab="Nearest")
plot(x=mydata$Scruz, y=densitylog, 
     ylab="Log of Density", 
     xlab="Scruz")
plot(x=mydata$Adjacent, y=densitylog, 
     ylab="Log of Density", 
     xlab="Adjacent")