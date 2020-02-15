# Time series data decomposition

# Trend
# Data import and time series plot
d = read.csv("AirPassengers.csv",head=T)
t = as.vector(t(d[,2]))
t = ts(t, start=1949, frequency=12)
ts.plot(t, ylab="Number of Passengers", main="Time Series Plot")

# Estimating trend by moving average method 
## Create equally spaced time points for fitting trends
time.pts = c(1:length(t))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
## Fit a moving average
library(accelerometry)
mav.fit = movingaves(x = t, window = 12)
t.fit.mav = ts(mav.fit, start=1950, frequency=12)
## Is there a trend? 
ts.plot(t, ylab="Number of Passengers", 
        main="Trend Line by Moving Average")
lines(t.fit.mav, lwd=2, col="purple")
abline(t.fit.mav[1], 0, lwd=2, col="blue")


# Estimating trend by parametric regression 
## fit the regression model
x1 = time.pts
x2 = time.pts^2
lm.fit = lm(t ~ x1+x2)
summary(lm.fit)
## Is there a trend? 
t.fit.lm = ts(fitted(lm.fit), start=1949, frequency=12)
ts.plot(t, ylab="Number of Passengers", 
        main="Trend Line by Parametric Regression")
lines(t.fit.lm, lwd=2, col="green")
abline(t.fit.lm[1], 0, lwd=2, col="blue")


# Estimating trend by local polynomial regression  
loc.fit = loess(t ~ time.pts)
t.fit.loc = ts(fitted(loc.fit), start=1949, frequency=12)
## Is there a trend? 
ts.plot(t, ylab="Number of Passengers", 
        main="Trend Line by Local Polynomial Regression")
lines(t.fit.loc, lwd=2, col="brown")
abline(t.fit.loc[1], 0, lwd=2, col="blue")


# Estimating trend by splines trend estimation 
library(mgcv)
gam.fit = gam(t ~ s(time.pts))
t.fit.gam = ts(fitted(gam.fit), start=1949, frequency=12)
## Is there a trend? 
ts.plot(t, ylab="Number of Passengers", 
        main="Trend Line by Splines Trend Estimation")
lines(t.fit.gam, lwd=2, col="red")
abline(t.fit.gam[1], 0, lwd=2, col="blue")


# Compare all estimated trends
all.val = c(t.fit.mav, t.fit.lm, t.fit.loc, t.fit.gam)
ylim = c(min(all.val), max(all.val))
ts.plot(t.fit.lm, lwd=2, col="green", 
        ylim=ylim, ylab="Number of Passengers", 
        main="Estimated Trends by Different Method")
lines(t.fit.mav, lwd=2, col="purple")
lines(t.fit.gam, lwd=2, col="red")
lines(t.fit.loc, lwd=2, col="brown")
legend(x=1950, y=450, legend=c("MAV","LM","GAM","LOESS"), 
       lty = 1, col = c("purple","green","red","brown"))



# Seasonality

# DATA EXPLORATION AND PROCESSING  
edvoldata = read.csv("EGDailyVolume.csv",header=T)
## Process Dates 
year = edvoldata$Year
month = edvoldata$Month
day = edvoldata$Day
datemat = cbind(as.character(day),
                as.character(month),
                as.character(year))
paste.dates = function(date){
  day = date[1]; month=date[2]; year = date[3]
  return(paste(day,month,year,sep="/"))
}
dates = apply(datemat,1,paste.dates)
dates = as.Date(dates, format="%d/%m/%Y")
edvoldata = cbind(dates,edvoldata)
attach(edvoldata)

plot(dates, Volume, type='l',
     ylab='Daily ED Volume', xlab='Time', 
     main='Line Plot of Original Volume')

## ED Volume is count data: Transform
Volume.tr = sqrt(Volume+3/8)
hist(Volume,nclass=20,
     xlab="ED Volume", col="brown",
     main='Histogram of Original Volume')
hist(Volume.tr,nclass=20,
     xlab= "Transformed ED Volume", col="blue",
     main='Histogram of Transformed Volume')
plot(dates, Volume.tr, type='l',
     ylab='Transformed Daily ED Volume',xlab='Time',
     main='Line Plot of Transformed Volume')


# TREND AND SEASONALITY ESTIMATION

library(mgcv)
time.pts = c(1:length(Volume))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
## Trend Estimation: Is there a trend?
## Local Polynomial Trend Estimation
loc.fit = loess(Volume.tr~time.pts)
vol.fit.loc = fitted(loc.fit)
## Splines Trend Estimation
gam.fit = gam(Volume.tr~s(time.pts))
summary(gam.fit)
vol.fit.gam = fitted(gam.fit)
## Is there a trend? 
plot(dates, sqrt(Volume+3/8), type='l',
     ylab='Transformed Daily ED Volume',xlab='Time',
     main='Estimated Trend Line')
lines(dates,vol.fit.loc,lwd=2,col="brown")
lines(dates,vol.fit.gam,lwd=2,col="red")
legend('topleft', legend=c("GAM","LOESS"), 
       lty = 1, col = c("red","brown"))

## Model Trend + Monthly Seasonality
## Using nonparametric trend and linear regression seasonality 
month = as.factor(format(dates,"%b"))
gam.fit.seastr.1 = gam(Volume.tr~s(time.pts)+month)
summary(gam.fit.seastr.1)
vol.fit.gam.seastr.1 = fitted(gam.fit.seastr.1)
plot(dates,sqrt(Volume+3/8), type='l',
     ylab='Transformed Daily ED Volume',xlab='Time',
     main='Estimated Trend and Seasonality Line')
lines(dates,vol.fit.gam.seastr.1,lwd=2,col="red")

## Add day-of-the-week seasonality
week = as.factor(weekdays(dates))
gam.fit.seastr.2 = gam(Volume.tr~s(time.pts)+month+week)
summary(gam.fit.seastr.2)
vol.fit.gam.seastr.2 = fitted(gam.fit.seastr.2)
## Compare the two fits: with & without day-of-the-week seasonality
plot(dates,vol.fit.gam.seastr.2, type='l',
     ylab="Seasonality and Trend: Daily ED Volume",xlab='Time',
     main='Estimated Trend and Seasonality Line')
lines(dates,vol.fit.gam.seastr.1,lwd=2,col="red")

## Does the addition of seasonality of day of the week adds predictive power?
lm.fit.seastr.1 = lm(Volume.tr~month)
lm.fit.seastr.2 = lm(Volume.tr~month+week)
anova(lm.fit.seastr.1,lm.fit.seastr.2)
vol.fit.lm.seastr.2 = fitted(lm.fit.seastr.2)
## Compare with & without trend
plot(dates,vol.fit.gam.seastr.2, type='l',
     ylab="Seasonality and Trend: Daily ED Volume",xlab='Time',
     main='Estimated Trend and Seasonality Line')
lines(dates,vol.fit.lm.seastr.2,lwd=2,col="blue")
lines(dates,vol.fit.gam,lwd=2,col="red")


# STATIONARITY: RESIDUAL PROCESS

## Residual Process: Trend Removal
resid.1 = Volume.tr-vol.fit.gam
## Residual Process: Stationarity Removal
resid.2 = Volume.tr-vol.fit.lm.seastr.2
## Residual Process: Trend & Stationarity Removal
resid.3 = Volume.tr-vol.fit.gam.seastr.2
y.min = min(c(resid.1,resid.2,resid.3))
y.max = max(c(resid.1,resid.2,resid.3))

plot(dates,resid.1, type='l',ylab="Residual Process",xlab='Time',
     main='Residual Plot')
lines(dates,resid.2,col="blue")
lines(dates,resid.3,col="brown")
legend('bottomleft',legend=c("Trend","Season","Trend+Season"),lty = 1,
       col=c("black","blue","brown"))

acf(resid.1,lag.max=12*4,main="ACF when Removing Trend")
acf(resid.2,lag.max=12*4,main="ACF when Removing Seasonality",
    col="blue")
acf(resid.3,lag.max=12*4,
    main="ACF when Removing Trend and Seasonality",col="brown")




# Stationary and ARMA
# Data exploration and processing  

edvoldata = read.csv("EGDailyVolume.csv",header=T)
## Process Dates
year = edvoldata$Year
month = edvoldata$Month
day = edvoldata$Day
datemat = cbind(as.character(day),as.character(month),as.character(year))
paste.dates = function(date){
  day = date[1]; month=date[2]; year = date[3]
  return(paste(day,month,year,sep="/"))
}
dates = apply(datemat,1,paste.dates)
dates = as.Date(dates, format="%d/%m/%Y")
edvoldata = cbind(dates,edvoldata)
attach(edvoldata)

## ED Volume is count data: Transform
Volume.tr = sqrt(Volume+3/8)

## Take the difference: weekly and yearly
volume.ts = ts(Volume.tr,start=c(2010,1,1),frequency=365.25)
dvolume7=diff(volume.ts,7) # weekly
dvolume12=diff(volume.ts,12) # monthly (annual)



# Plot time series and difference processes  

par(mfrow=c(2,2))
ts.plot(volume.ts,ylab="ED Volume")
ts.plot(dvolume7,ylab="Weekly difference")
ts.plot(dvolume12,ylab="Yearly difference")

par(mfrow=c(2,2))
acf(as.vector(volume.ts), main='Time Series: ACF',lag.max=360*2)
acf(as.vector(volume.ts),type="partial", main='Time Series: PACF',lag.max=360*2)
acf(as.vector(dvolume7) , main='Weekly Diffference:ACF',lag.max=360*2)
acf(as.vector(dvolume12), main='Yearly Difference: ACF',lag.max=360*2)


# Model fitting ARIMA(5,1,5)+seasonal ARMA(1,1) 

mod = arima(volume.ts, order = c(5,1,5),
            seasonal = list(order = c(1,0,1),period=7),
            method = "ML")

# residual analysis
plot(resid(mod), ylab='Standardized Residuals',
     type='o',main="Residual Plot")
abline(h=0)
acf(as.vector(resid(mod)),lag.max=365*2,
    main="ACF: Residuals")
hist(resid(mod),xlab='Standardized Residuals',
     main='Histogram: Residuals')
qqnorm(resid(mod))
qqline(resid(mod))



# Forecasting with ARIMA: 2 weeks ahead  

n = length(volume.ts)
nfit = n-14
outvol = arima(volume.ts[1:nfit], 
               order = c(5,1,5),
               seasonal = list(order = c(1,0,1),period=7),
               method = "ML",
               optim.control=list(maxit=1000))
out_pred = as.vector(predict(outvol,n.ahead=14))

timevol=time(volume.ts)
ubound = out_pred$pred+1.96*out_pred$se
lbound = out_pred$pred-1.96*out_pred$se
ymin = min(lbound)
ymax = max(ubound)
par(mfrow=c(1,1))
plot(timevol[(n-56):n],volume.ts[(n-56):n],
     type="l", ylim=c(ymin,ymax), xlab="Time", 
     ylab="ED Volume")
points(timevol[(nfit+1):n],out_pred$pred,col="red")
lines(timevol[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(timevol[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")

