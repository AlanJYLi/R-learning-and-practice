## Part 1: Data Loading
#Read dataframe
whole.df = read.csv('ESS8e02.1_F1.csv', head=T)

# focus on how political attitude and behavior influence subjective happiness
# include basic demographic variables as control variables (e.g gender, age, education level etc.)
variables = c("happy","gndr","agea","edulvlb",
              "hinctnta","pdwrk","health","psppsgva",
              "actrolga","psppipla","cptppola","trstprl",
              "trstlgl","trstplc","trstplt","trstprt","trstep",
              "trstun", "stflife","stfeco","stfgov","stfdem","stfedu",
              "stfhlth", "contplt","wrkprty","wrkorg","badge",
              "sgnptit","pbldmn","bctprd","pstplonl")
target.df = whole.df[,variables]


## Part 2: Data Cleaning  
# Explore the Missing Value

# response variable
target.df[target.df$happy == 77, 'happy'] = -1
target.df[target.df$happy == 88, 'happy'] = -1
target.df[target.df$happy == 99, 'happy'] = -1
target.df[target.df$happy == -1, 'happy'] = NA

# gender
target.df[target.df$gndr == 9, 'gndr'] = NA

# age
target.df[target.df$agea == 999, 'agea'] = NA

# level of education
target.df[target.df$edulvlb == 5555, 'edulvlb'] = -1
target.df[target.df$edulvlb == 7777, 'edulvlb'] = -1
target.df[target.df$edulvlb == 8888, 'edulvlb'] = -1
target.df[target.df$edulvlb == 9999, 'edulvlb'] = -1
target.df[target.df$edulvlb == -1, 'edulvlb'] = NA

# household income
target.df[target.df$hinctnta == 77, 'hinctnta'] = -1
target.df[target.df$hinctnta == 88, 'hinctnta'] = -1
target.df[target.df$hinctnta == 99, 'hinctnta'] = -1
target.df[target.df$hinctnta == -1, 'hinctnta'] = NA

# health
target.df[target.df$health == 7, 'health'] = -1
target.df[target.df$health == 8, 'health'] = -1
target.df[target.df$health == 9, 'health'] = -1
target.df[target.df$health == -1, 'health'] = NA

# political related behavior
politicalBehavior_feature = c("contplt","wrkprty","wrkorg",
                              "badge","sgnptit","pbldmn",
                              "bctprd","pstplonl")
for (feature in politicalBehavior_feature) {
  print(feature)
  print(table(target.df[feature]))
  target.df[target.df[feature] == 7, feature] = -1
  target.df[target.df[feature] == 8, feature] = -1
  target.df[target.df[feature] == 9, feature] = -1
  target.df[target.df[feature] == -1, feature] = NA
}

# 5 points scale(1-5) variables; missing values: 7, 8, 9
var_5points = c("psppsgva","actrolga","psppipla","cptppola")
for (feature in var_5points) {
  target.df[target.df[feature] == 7, feature] = -1
  target.df[target.df[feature] == 8, feature] = -1
  target.df[target.df[feature] == 9, feature] = -1
  target.df[target.df[feature] == -1, feature] = NA
}

# 11 points scale(0-10) variables; missing values: 77, 88, 99
var_11points = c("trstprl","trstlgl","trstplc","trstplt",
                 "trstprt","trstep","trstun","stflife",
                 "stfeco","stfgov","stfdem","stfedu","stfhlth")
for (feature in var_11points) {
  target.df[target.df[feature] == 77, feature] = -1
  target.df[target.df[feature] == 88, feature] = -1
  target.df[target.df[feature] == 99, feature] = -1
  target.df[target.df[feature] == -1, feature] = NA
}

# number of missing value in each features
sapply(target.df, function(x) sum(is.na(x)))

# number of missing value for each participant
target.df$na_count = apply(target.df, 1, function(x) sum(is.na(x)))
table(target.df$na_count)



# Data Cleaning

# omit the participants whose number of missing value is larger than 5
target.df = target.df[target.df$na_count <= 5,]


# Feature Exploration/Reduction and Modification
# "psppsgva","actrolga","psppipla","cptppola"

var_5points = c("psppsgva","actrolga","psppipla","cptppola")

# impute missing value by mode
getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

for (feature in var_5points) {
  v = target.df[is.na(target.df[feature]) == F, feature]
  target.df[is.na(target.df[feature]) == T, feature] = getmode(v)
}

library(plyr)
target.df$psppsgva = factor(target.df$psppsgva)
target.df$psppsgva = revalue(target.df$psppsgva, c('1'='Not at all', 
                                                   '2'='Very little', 
                                                   '3'='Some', 
                                                   '4'='A lot', 
                                                   '5'='A great deal'))

target.df$actrolga = factor(target.df$actrolga)
target.df$actrolga = revalue(target.df$actrolga, c('1'='Not at all able', 
                                                   '2'='Very little able', 
                                                   '3'='Quite able', 
                                                   '4'='Very able', 
                                                   '5'='Completely able'))

target.df$psppipla = factor(target.df$psppipla)
target.df$psppipla = revalue(target.df$psppipla, c('1'='Not at all', 
                                                   '2'='Very little', 
                                                   '3'='Some', 
                                                   '4'='A lot', 
                                                   '5'='A great deal'))

target.df$cptppola = factor(target.df$cptppola)
target.df$cptppola = revalue(target.df$cptppola, c('1'='Not at all confident', 
                                                   '2'='a little confident', 
                                                   '3'='Quite confident', 
                                                   '4'='Very confident', 
                                                   '5'='Completely confident'))

# Feature Exploration/Reduction and Modification
# 11 points scale(0-10) variables

# Following other researches using European Social Survey data, 
# "trstprl","trstlgl","trstplc","trstplt","trstprt","trstep", 
# and "trstun" are scores measuring individual's trust 
# to different political entities. And the total score is 
# considered as the general score of political trust

# Similar, "stflife","stfeco","stfgov","stfdem","stfedu", 
# and "stfhlth" are scores measuring individual's satisfaction 
# to different political issues. And the total score is 
# considered as the general score of political satisfaction

var_11points = c("trstprl","trstlgl","trstplc","trstplt",
                 "trstprt","trstep","trstun","stflife",
                 "stfeco","stfgov","stfdem","stfedu","stfhlth")

# impute the missing value by useing mean
for (feature in var_11points) {
  v = target.df[is.na(target.df[feature]) == F, feature]
  target.df[is.na(target.df[feature]) == T, feature] = mean(v)
}

# calculate the total score
trust_feature = c("trstprl","trstlgl","trstplc",
                  "trstplt","trstprt","trstep","trstun")
satisfaction_feature = c("stflife","stfeco","stfgov",
                         "stfdem","stfedu","stfhlth")
target.df['generaltrust'] = rowSums(target.df[trust_feature])
target.df['generalsatisfaction'] = rowSums(target.df[satisfaction_feature])


# Feature Exploration/Reduction and Modification-household income

# add a new category called "missing"
target.df[is.na(target.df$hinctnta)==T,'hinctnta'] = -1
target.df$hinctnta = factor(target.df$hinctnta)
target.df$hinctnta = revalue(target.df$hinctnta, c('1'='0-10 percentile',
                                                   '2'='10-20 percentile',
                                                   '3'='20-30 percentile',
                                                   '4'='30-40 percentile',
                                                   '5'='40-50 percentile',
                                                   '6'='50-60 percentile',
                                                   '7'='60-70 percentile',
                                                   '8'='70-80 percentile',
                                                   '9'='80-90 percentile',
                                                   '10'='90-100 percentile',
                                                   '-1'='missing'))
summary(target.df$hinctnta)


# Feature Exploration/Reduction and Modification - gender

target.df = target.df[is.na(target.df$gndr) == F,] # remove missing values
target.df$gndr = factor(target.df$gndr)
target.df$gndr = revalue(target.df$gndr, c('1'= 'Male', '2' = 'Female'))
summary(target.df$gndr)


# Feature Exploration/Reduction and Modification - age
target.df = target.df[is.na(target.df$agea) == F,] # remove missing values
labs = c(paste(seq(0,80, by=10), seq(0+10-1, 90-1, by=10), sep='-'), paste(90,'+',sep =''))
target.df$agegroup = cut(target.df$agea, breaks = c(seq(0,90, by=10), Inf), labels = labs, right = FALSE)
target.df$agegroup = factor(target.df$agegroup)
summary(target.df$agegroup)


# Feature Exploration/Reduction and Modification - education
target.df = target.df[is.na(target.df$edulvlb) == F,]
target.df$edulvlb = factor(target.df$edulvlb)
target.df$edulvlb = revalue(target.df$edulvlb, c('0'='Pre-Primary',
                                                 '113'='Primary',
                                                 '129'='Primary',
                                                 '212'='Lower Secondary',
                                                 '213'='Lower Secondary',
                                                 '221'='Lower Secondary',
                                                 '222'='Lower Secondary',
                                                 '223'='Lower Secondary',
                                                 '229'='Lower Secondary',
                                                 '311'='Upper Secondary',
                                                 '312'='Upper Secondary',
                                                 '313'='Upper Secondary',
                                                 '321'='Upper Secondary',
                                                 '322'='Upper Secondary',
                                                 '323'='Upper Secondary',
                                                 '412'='Vocational',
                                                 '413'='Vocational',
                                                 '421'='Vocational',
                                                 '422'='Vocational',
                                                 '423'='Vocational',
                                                 '510'='Associate',
                                                 '520'='Associate',
                                                 '610'='Bachelor',
                                                 '620'='Bachelor',
                                                 '710'='Master',
                                                 '720'='Master',
                                                 '800'='Doctoral'))
summary(target.df$edulvlb)


# Feature Exploration/Reduction and Modification - paid work
target.df$pdwrk = factor(target.df$pdwrk)
target.df$pdwrk = revalue(target.df$pdwrk, c('0' = 'No', '1' = 'Yes'))
summary(target.df$pdwrk)

# Feature Exploration/Reduction and Modification - health self report
target.df = target.df[is.na(target.df$health) == F,]
target.df$health = factor(target.df$health)
target.df$health = revalue(target.df$health, c('1'='Very Good', 
                                               '2'='Good', 
                                               '3'='Fair', 
                                               '4'='Bad', 
                                               '5'='Very Bad'))
summary(target.df$health)


# Feature Exploration/Reduction and Modification - Polical Involvement
# how many political related behavior the person conducted in past one year
# yes-1 or no-2
politicalBehavior_feature = c("contplt","wrkprty","wrkorg",
                              "badge","sgnptit","pbldmn",
                              "bctprd","pstplonl")

for (feature in politicalBehavior_feature) {
  target.df = target.df[is.na(target.df[feature]) ==F,]
  target.df[target.df[feature]==2, feature] = 0
}

target.df['behaviorinvolvment'] = rowSums(target.df[politicalBehavior_feature])

target.df$behaviorinvolvment = factor(target.df$behaviorinvolvment)
target.df$behaviorinvolvment = revalue(target.df$behaviorinvolvment, 
                                       c('0'='Not at all', 
                                         '1'='Low level',
                                         '2'='Low level',
                                         '3'='Median level',
                                         '4'='Median level',
                                         '5'='Median level',
                                         '6'='High level',
                                         '7'='High level',
                                         '8'='High level'))


# Feature Exploration/Reduction and Modification - response variable
# 11 points scale(0-10)
target.df = target.df[is.na(target.df$happy) == F,]
target.df$happy = factor(target.df$happy)
summary(target.df$happy)


final_variable_list = c("happy","gndr","agegroup","edulvlb",
                        "hinctnta","pdwrk","health","psppsgva","actrolga",
                        "psppipla","cptppola","generaltrust",
                        "generalsatisfaction", "behaviorinvolvment")

data = target.df[final_variable_list]

summary(data)


## Part 3: Exploratory Data Analysis

# Exploratory data analysis: Response vs Categorical Predictors-gender
attach(data)
tb = xtabs(~gndr+happy)
barplot(prop.table(tb),axes=T,space=0.3,legend.text=TRUE,ylim=c(0,0.5),
        xlab="Happiness Score", ylab = "Proportion",
        horiz=F, col=c("red","blue"),main="Happiness by Gender")

# Exploratory data analysis: Response vs Categorical Predictors-age group
library(vcd)
tb = xtabs(~happy+agegroup)
mosaicplot(tb, xlab="Happiness", ylab="Age Group", 
           main="Happiness by Age Group",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Categorical Predictors-education level
tb = xtabs(~happy+edulvlb)
mosaicplot(tb, xlab="Happiness", ylab="Education Level", 
           main="Happiness by Education Level",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Categorical Predictors-household income
tb = xtabs(~happy+hinctnta)
mosaicplot(tb, xlab="Happiness", ylab="Household Income Level", 
           main="Happiness by Household Income Level",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Categorical Predictors-worked or not
tb = xtabs(~pdwrk+happy)
barplot(prop.table(tb),axes=T,space=0.3,legend.text=TRUE,ylim=c(0,0.5),
        xlab="Happiness Score", ylab = "Proportion",
        horiz=F, col=c("red","blue"),main="Happiness by Worked or Not")

# Exploratory data analysis: Response vs Categorical Predictors-health
tb = xtabs(~happy+health)
mosaicplot(tb, xlab="Happiness", ylab="Health", 
           main="Happiness by Health Level",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Categorical Predictors
# item: Political system allows people to have a say in what government does
tb = xtabs(~happy+psppsgva)
mosaicplot(tb, xlab="Happiness", ylab="Allowing Opinion Expression", 
           main="Happiness by Attitude",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Categorical Predictors
# item: Able to take active role in political group
tb = xtabs(~happy+actrolga)
mosaicplot(tb, xlab="Happiness", ylab="Able to Take Active Role", 
           main="Happiness by Attitude",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Categorical Predictors
# item: Political system allows people to have influence on politics
tb = xtabs(~happy+psppipla)
mosaicplot(tb, xlab="Happiness", ylab="Allowing Influence", 
           main="Happiness by Attitude",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Categorical Predictors
# item: Confident in own ability to participate in politics
tb = xtabs(~happy+psppipla)
mosaicplot(tb, xlab="Happiness", ylab="Confident in Politics Participating Ability", 
           main="Happiness by Attitude",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Categorical Predictors
# political behavior involvement
tb = xtabs(~happy+behaviorinvolvment)
mosaicplot(tb, xlab="Happiness", ylab="Political Behavior Involvement Level", 
           main="Happiness by Political Behavior Involvement Level",
           color=TRUE, legend=T, shade=T, las=1)

# Exploratory data analysis: Response vs Numeric Predictors-generaltrust
boxplot(generaltrust~happy, 
        main="General Trust Score in Different Happiness Level")

# Exploratory data analysis: Response vs Numeric Predictors-generalsatisfaction
boxplot(generalsatisfaction~happy, 
        main="General Satisfaction Score in Different Happiness Level")

# Exploratory data analysis: 
# scatter plot between generaltrust and generalsatisfaction
plot(generaltrust,generalsatisfaction, 
     main="Scatter Plot of General Trust and Satisfatction")
r = round(cor(generaltrust,generalsatisfaction), digits=2)
txt = paste('r=',r)
par(usr = c(0, 0.6, 0.3, 6))
text(0.5, 0.9, txt, col='red')

#Correlation score:
cor(as.matrix(cbind(generaltrust,generalsatisfaction)))


# !!!!!If it's not necessary, don't run this part. It may take about half an hour.
# Exploratory data analysis: 
# pairedwise correlation coefficients between ordinal features
# Kendall's Tau-b

m = cbind(as.numeric(data$health), as.numeric(data$psppsgva), 
          as.numeric(data$actrolga), as.numeric(data$psppipla), 
          as.numeric(data$cptppola), as.numeric(data$behaviorinvolvment))
colnames(m) = c("health", "psppsgva", 
                "actrolga", "psppipla", 
                "cptppola", "behaviorinvolvment")

cor(m, method="kendall", use="pairwise") 


# Most of the coefficients is under 0.4. The largest one is around 0.6.   


## Part 4: Model Fitting

# Response variable is a ordinal discreet variable
# and predicting variables include both qualitative and quantitative variavles. 
# In addition, the response variable follows an order from low to high in which a score of 10 is extremely happy while score of 0 is extremely sad. 
# Hence, We will use ordinal logistic regression to estimate/ predict the multi-class ordered variable "happy". 


library(MASS)
library(PResiduals)
library(sure)
library(ggplot2)

# Fit models with various link functions to the simulated data
fit.probit <- polr(happy ~ ., data = data, method = "probit")
fit.logistic <- polr(happy ~ ., data = data, method = "logistic")
fit.loglog <- polr(happy ~ ., data = data, method = "loglog")
fit.cloglog <- polr(happy ~ ., data = data, method = "cloglog")

# Construct Q-Q plots of the surrogate residuals for each model
set.seed(1056)  # for reproducibility
p1 <- autoplot(fit.probit, nsim = 100, what = "qq", main = 'abc')
p2 <- autoplot(fit.logistic, nsim = 100, what = "qq")
p3 <- autoplot(fit.loglog, nsim = 100, what = "qq")
p4 <- autoplot(fit.cloglog, nsim = 100, what = "qq")

# plot
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)  

# Fit the full model using Ordinal Logistic Regression and logit function
full.model = polr(as.ordered(happy)~., data = data, Hess = TRUE, method = "logistic")
summary(full.model)

# Statistical Inference of coefficients
summary_table <- coef(summary(full.model))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table



## Part 5: Goodness of Fit

#Method 1: Visual Analysis - Assumption Evaluation

# Assumption 3: Multicollinearity

regression.line = lm(as.numeric(happy) ~ ., data = data)

library(car)
max(10,1/(1-summary(regression.line)$r.squared))
vif(regression.line)
# Assumption 4: Proportional Odds/ Parallel Lines

library(brant)
brant(full.model,by.var=F)
# Others: Residual Computation

# for residual function and sample data sets
library(sure)  

# for reproducibility
set.seed(1056)  
sres <- resids(full.model) #Residual Computation


# Independence & Linearity - Residuals vs each predictor

par(mfrow=c(1,2))
# Scatter plot of numerical variable "General Trust"
plot(generaltrust,sres,xlab="General Trust",ylab="Surrogate Residuals")
abline(0,0,col="red")

# Scatter plot of numerical variable "General Satisfaction"
plot(generalsatisfaction,sres,xlab="General Satisfaction",ylab="Surrogate Residuals")
abline(0,0,col="red")

par(mfrow=c(2,2))
# Box plot of residual vs categorical variable "gender"
boxplot(sres~gndr,ylab = "Surrogate residuals",xlab='Gender')

# Box plot of residual vs categorical variable "Age group"
boxplot(sres~agegroup,ylab = "Surrogate residuals",xlab='Age Group')

# Box plot of residual vs categorical variable "Highest education level"
boxplot(sres~edulvlb,ylab = "Surrogate residuals",xlab='Highest Education Level')

# Box plot of residual vs categorical variable "Household's Net Income"
boxplot(sres~hinctnta,ylab = "Surrogate residuals",xlab="Household's Net Income")

par(mfrow=c(2,2))
# Box plot of residual vs categorical variable "Paid Work"
boxplot(sres~pdwrk, ylab = "Surrogate residuals", xlab='Paid Work')

# Box plot of residual vs categorical variable "Say in Government"
boxplot(sres~psppsgva, ylab = "Surrogate residuals", xlab='Say in Government')

# Box plot of residual vs categorical variable "Active Political Role"
boxplot(sres~actrolga, ylab = "Surrogate residuals", xlab='Active Political Role')

# Box plot of residual vs categorical variable "Government allows people influence"
boxplot(sres~psppipla, ylab = "Surrogate residuals", xlab="Government allows people influence")

par(mfrow=c(1,2))
# Box plot of residual vs categorical variable "Political Confidence"
boxplot(sres~cptppola, ylab = "Surrogate residuals", xlab='Political Confidence')

# Box plot of residual vs categorical variable "Behavioral Involvement"
boxplot(sres~behaviorinvolvment, ylab = "Surrogate residuals", xlab='Behavioral Involvement')


# Normality - Q-Q plot & histogram of residuals

qqPlot(sres, ylab="Residuals", main = "")
hist(sres, xlab="Residuals", main = "",nclass=10,col="orange")

# Method 2: Hypothesis Test

#With deviance residuals
1-pchisq(full.model$deviance,full.model$df.residual)


## Part 5: Variable Selection

# Different methods for variable selection

# Forward Stepwise Regression

# Apply Stepwise Forward Regression for variable selection

minmod = polr(happy~gndr+agegroup+edulvlb+hinctnta+pdwrk+health, data = data, Hess = TRUE)
forward.model=step(minmod, scope = list(lower=minmod,upper=full.model), direction = "forward",trace=T)
summary(forward.model)
# Backward Stepwise Regression
# Apply Stepwise Backward Regression for variable selection

minmod = polr(happy~gndr+agegroup+edulvlb+hinctnta+pdwrk+health, data = data, Hess = TRUE)
backward.model=step(full.model, scope = list(lower=minmod,upper=full.model), direction = "backward",trace=T)
summary(backward.model)
# LASSO Regression
# !!!!!!!If it's not necessary, don't run lasso regression here. It may take around one hour to run this part
# Apply LASSO for variable selection.
# Perform Lasso regression. Find the optimal lambda value using 10 fold CV.
library(ordinalNet)
predictors = model.matrix(happy ~ ., data = data)
predictors = predictors[,-1]
response = as.factor(data$happy)

# This function automatically scales the predicotrs.
lasso.cv = ordinalNetTune(predictors,response,
                          alpha = 1,nFolds = 10,
                          family = c("cumulative"), link = c("logit"))

# find the lambda value with lowest BIC
minbic = which(lasso.cv[["fit"]][["bic"]] == min(lasso.cv[["fit"]][["bic"]]))
lambdavalue = lasso.cv[["lambdaVals"]][minbic]
lasso = ordinalNet(predictors, response,
                   alpha = 1, lambdaVals = c(lambdavalue),
                   family = c("cumulative"), link = c("logit"))

# coefficients of lasso regression model
lasso[["coefs"]]
summary(lasso)

## Part 6: Subsampling
#Confidence Intervals with subsampling

library(MASS)
n= nrow(data)
subsample = sample(n, ceiling(n*0.2), replace = FALSE) 
subdata = data[subsample,]
submodel = polr(happy~., data = subdata, Hess = TRUE, method = "logistic")

summary(submodel)

CI = confint(submodel)
CI


library(MASS)
n= nrow(data)
subsample2 = sample(n, ceiling(n*0.2), replace = FALSE) 
subdata2 = data[subsample2,]
second.submodel = polr(happy~., data = subdata2, Hess = TRUE, method = "logistic")

summary(second.submodel)

second.CI = confint(second.submodel)
second.CI

library(MASS)
n= nrow(data)
subsample3 = sample(n, ceiling(n*0.2), replace = FALSE) 
subdata3 = data[subsample3,]
third.submodel = polr(happy~., data = subdata3, Hess = TRUE, method = "logistic")

summary(third.submodel)

third.CI = confint(third.submodel)
third.CI


library(MASS)
n= nrow(data)
subsample4 = sample(n, ceiling(n*0.2), replace = FALSE) 
subdata4 = data[subsample4,]
fourth.submodel = polr(happy~., data = subdata4, Hess = TRUE, method = "logistic")


summary(fourth.submodel)


fourth.CI = confint(fourth.submodel)
fourth.CI

library(MASS)
n= nrow(data)
subsample5 = sample(n, ceiling(n*0.2), replace = FALSE) 
subdata5 = data[subsample5,]
fifth.submodel = polr(happy~., data = subdata5, Hess = TRUE, method = "logistic")

summary(fifth.submodel)


fifth.CI = confint(fifth.submodel)
fifth.CI

#Variable Selection using Subsampling
library(rlist)
variable.df=(names(full.model$coefficients))

n = nrow(data)
i=1
while (i<101) {
  
  set.seed(Sys.time())
  
  subsample.vs = sample(n, ceiling(n*0.2), replace = FALSE) 
  subdata.vs = data[subsample.vs,]
  submodel.vs = polr(happy~., data = subdata.vs, Hess = TRUE, method = "logistic")
  
  minmod.sub = polr(happy~gndr+agegroup+edulvlb+hinctnta+pdwrk+health, data = subdata.vs, Hess = TRUE)
  
  model.forwardvs=step(minmod.sub, scope = list(lower=minmod.sub,upper=submodel.vs), direction = "forward",trace=FALSE)
  
  
  variable.df=list.append(variable.df, names(model.forwardvs$coefficients))
  
  print(i)
  
  i = i+1
}


names(model.forwardvs$coefficients)
names(full.model$coefficients)

library(rlist)
variable.df=list.append(variable.df, names(model.forwardvs$coefficients))
variable.df

gndrFemale.count = length(which(variable.df == 'gndrFemale'))

agegroup20.count = length(which(variable.df == 'agegroup20-29'))

agegroup30.count = length(which(variable.df == 'agegroup30-39'))

agegroup40.count = length(which(variable.df == 'agegroup40-49'))

agegroup50.count = length(which(variable.df == 'agegroup50-59'))

agegroup60.count = length(which(variable.df == 'agegroup60-69'))

agegroup70.count = length(which(variable.df == 'agegroup70-79'))

agegroup80.count = length(which(variable.df == 'agegroup80-89'))             

agegroup90.count = length(which(variable.df == 'agegroup90+'))

edulvlPrimary.count = length(which(variable.df == 'edulvlbPrimary'))

edulvlLowerSecondary.count = length(which(variable.df =='edulvlbLower Secondary')) 

edulvlUpperSecondary.count = length(which(variable.df =='edulvlbUpper Secondary'))

edulvlVocation.count = length(which(variable.df =='edulvlbVocational'))

edulvlAssociate.count = length(which(variable.df == 'edulvlbAssociate'))        

edulvlBachelor.count = length(which(variable.df =='edulvlbBachelor'))            

edulvlMaster.count = length(which(variable.df =='edulvlbMaster'))              

edulvlDoctoral.count = length(which(variable.df=='edulvlbDoctoral'))            

hinctnta10.count = length(which(variable.df=='hinctnta10-20 percentile'))     

hinctnta20.count = length(which(variable.df == 'hinctnta20-30 percentile'))   

hinctnta30.count = length(which(variable.df == 'hinctnta30-40 percentile'))

hinctnta40.count = length(which(variable.df == 'hinctnta40-50 percentile'))

hinctnta50.count = length(which(variable.df == 'hinctnta50-60 percentile'))      

hinctnta60.count = length(which(variable.df == 'hinctnta60-70 percentile'))     

hinctnta70.count = length(which(variable.df == 'hinctnta70-80 percentile'))      

hinctnta80.count = length(which(variable.df == 'hinctnta80-90 percentile'))      

hinctnta90.count = length(which(variable.df == 'hinctnta90-100 percentile'))     

hinctntamissing.count = length(which(variable.df == 'hinctntamissing'))           

pdwrkYes.count = length(which(variable.df == 'pdwrkYes'))                      

healthGood.count = length(which(variable.df == 'healthGood'))                

healthFair.count = length(which(variable.df == 'healthFair'))                    

healthBad.count = length(which(variable.df == 'healthBad'))                  

healthVeryBad.count = length(which(variable.df == 'healthVery Bad'))  

psppsgvaLittle.count = length(which(variable.df == 'psppsgvaVery little' ))        

psppsgvaSome.count = length(which(variable.df == 'psppsgvaSome'))                  

psppsgvaAlot.count = length(which(variable.df == 'psppsgvaA lot'))                

psppsgvaGreatDeal.count = length(which(variable.df == 'psppsgvaA great deal'))    

actrolgaVeryLittleAble.count = length(which(variable.df =='actrolgaVery little able'))

actrolgaQuiteAble.count = length(which(variable.df =='actrolgaQuite able'))

actrolgaVeryAble.count = length(which(variable.df == 'actrolgaVery able'))

actrolgaCompletelyAble.count = length(which(variable.df == 'actrolgaCompletely able'))

psppiplaVeryLittle.count = length(which(variable.df == 'psppiplaVery little'))    

psppiplaSome.count = length(which(variable.df == 'psppiplaSome'))                  

psppiplaAlot.count = length(which(variable.df == 'psppiplaA lot'))               

psppiplaAgreatdeal.count = length(which(variable.df == 'psppiplaA great deal'))   

cptppolaLittleConfident.count = length(which(variable.df =='cptppolaa little confident'))   

cptppolaQuiteConfident.count = length(which(variable.df == 'cptppolaQuite confident'))       

cptppolaVeryConfident.count = length(which(variable.df == 'cptppolaVery confident'))   

cptppolaCompletelyConfident.count = length(which(variable.df =='cptppolaCompletely confident'))

generalTrust.count = length(which(variable.df == 'generaltrust'))        

generalSatisfaction.count = length(which(variable.df == 'generalsatisfaction'))   

behaviorinvolvmentLow.count = length(which(variable.df == 'behaviorinvolvmentLow level'))

behaviorinvolvmentMedian.count = length(which(variable.df == 'behaviorinvolvmentMedian level'))

behaviorinvolvmentHigh.count = length(which(variable.df == 'behaviorinvolvmentHigh level'))

psppsgva.count = max(psppsgvaAlot.count,
                     psppsgvaGreatDeal.count,
                     psppiplaSome.count,
                     psppiplaVeryLittle.count)


actrolga.count = max(actrolgaCompletelyAble.count, 
                     actrolgaQuiteAble.count, 
                     actrolgaVeryAble.count, 
                     actrolgaVeryLittleAble.count)

psppipla.count = max(psppiplaAgreatdeal.count,
                     psppiplaAlot.count,
                     psppiplaSome.count,
                     psppiplaVeryLittle.count,
                     psppiplaVeryLittle.count)


cptppola.count = max(cptppolaCompletelyConfident.count,
                     cptppolaLittleConfident.count,
                     cptppolaQuiteConfident.count,
                     cptppolaVeryConfident.count)

behaviorinvolvment.count = max(behaviorinvolvmentHigh.count,
                               behaviorinvolvmentMedian.count,
                               behaviorinvolvmentLow.count)

subvariable.names = c('psppsgva', 
                      'actrolga', 
                      'psppipla', 
                      'cptppola', 
                      'Trust', 
                      'Satisfaction', 
                      'Involvement')

subvariable.values = c(psppipla.count, 
                       actrolga.count, 
                       psppipla.count, 
                       cptppola.count, 
                       generalTrust.count, 
                       generalSatisfaction.count, 
                       behaviorinvolvment.count)

names(subvariable.values) = subvariable.names

barplot(subvariable.values, main='Subsample Step Regression Frequency', ylab ='Frequency (%)', las=2, cex.names = .7, col = grey.colors(7, start=1, end=.3))

#Subsampling for distribution of p-values

pvalue.matrix = matrix(0,nrow = 53,ncol = 100)

n = nrow(data)
i=1
while (i<101) {
  
  set.seed(Sys.time())
  
  subsample.p = sample(n, ceiling(n*0.2), replace = FALSE) 
  subdata.p = data[subsample.p,]
  submodel.p = polr(happy~., data = subdata.p, Hess = TRUE, method = "logistic")
  
  summary.p = coef(summary(submodel.p))
  pval = pnorm(abs(summary.p[, "t value"]),lower.tail = FALSE)* 2
  summary.p = cbind(summary.p, "p value" = round(pval,3))
  
  pvalue.matrix[,i] = summary.p[1:53,4]
  
  
  i = i+1
}


library(car)

par(mfrow=c(2,2))
for (i in 29:53) {
  qqPlot(as.numeric(submodel.pmatrix[i,2:101]), distribution='unif', ylim=c(0,1), ylab='Sample  Quantiles', xlab='Distribution Quantiles', main=submodel.pmatrix[i,1])
}


#Goodness of Fit
library(brant)
brant.list=brant.submodel[1,3]

n = nrow(data)
i=1
while (i<101) {
  
  set.seed(Sys.time())
  
  subsample.brant = sample(n, ceiling(n*0.2), replace = FALSE) 
  subdata.brant = data[subsample.brant,]
  submodel.brant = polr(happy~., data = subdata.brant, Hess = TRUE, method = "logistic")
  
  
  brant.submodel = brant(submodel.brant)
  brant.list = list.append(brant.list, brant.submodel[1,3])
  
  
  i = i+1
}

mean(brant.list)

1-pchisq(submodel.fullmod$deviance, submodel.fullmod$df.residual)
1-pchisq(reduced.vsmod$deviance,reduced.vsmod$df.residual)
