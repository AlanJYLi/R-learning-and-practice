# Missing data imputation

rawData = read.csv("breast-cancer-wisconsin.data.txt",header=F)
for (i in seq(2,ncol(rawData))) {
  print(table(rawData[,i]))
  print(sum(table(rawData[,i])))
}

rawData = read.csv("breast-cancer-wisconsin.data.txt",header=F,na.string="?")
colnames(rawData)=c("ID","ClumpThickness","UniformityofCellSize",
                    "UniformityofCellShape","MarginalAdhesion",
                    "SingleEpithelialCellSize","BareNuclei",
                    "BlandChromatin","NormalNucleoli",
                    "Mitoses","Class")
rawData[rawData[,"Class"]==2,"Class"]="benign"
rawData[rawData[,"Class"]==4,"Class"]="malignant"
rawData$Class=as.factor(rawData$Class)
summary(rawData)


# Use the mean/mode imputation method to impute values for the missing data
cat("frequency:");table(rawData[,"BareNuclei"])

# use mode
dataMode=rawData
dataMode[is.na(dataMode[,"BareNuclei"])==T,"BareNuclei"]=1
summary(dataMode[,"BareNuclei"])

# use mean
dataMean=rawData
dataMean[is.na(dataMean[,"BareNuclei"])==T,"BareNuclei"]=
  mean(dataMean[,"BareNuclei"], na.rm=TRUE)
summary(dataMean[,"BareNuclei"])


# Use regression to impute values for the missing data
library(mice)
dataReg=rawData
model1=mice(dataReg[,2:11], method="norm.predict", m=1, maxit=1, seed=1234)
model1$imp$BareNuclei
dataReg[,2:11]=complete(model1)


# Use regression with perturbation to impute values for the missing data
library(mice)
dataRegPer=rawData
model2=mice(dataRegPer[,2:11], method="norm.nob", m=1, maxit=1, seed=1234)
model2$imp$BareNuclei
plot(model1$imp$BareNuclei[,1], model2$imp$BareNuclei[,1],
     xlim=c(0,10), ylim=c(0,10),
     xlab="Regression", ylab="Regression with Perturbation")
abline(a=0, b=1, col="red")

cat("out of range values:\n");sum(model2$imp$BareNuclei<1 | model2$imp$BareNuclei>10)
dataRegPer[,2:11]=complete(model2)
dataRegPerRetain=dataRegPer
dataRegPerRound=dataRegPer
dataRegPerRound[dataRegPerRound$BareNuclei<1,"BareNuclei"]=1
dataRegPerRound[dataRegPerRound$BareNuclei>10,"BareNuclei"]=10


# Compare the results and quality of classification models
dataRemove=rawData
dataRemove=na.omit(dataRemove)

dataBinary=rawData
dataBinary$Missing=rep(1,nrow(dataBinary)) # 0=missing, 1=not
dataBinary[is.na(dataBinary$BareNuclei)==T, "Missing"]=0

dependents=c("ClumpThickness","UniformityofCellSize",
             "UniformityofCellShape","MarginalAdhesion",
             "SingleEpithelialCellSize","BlandChromatin",
             "NormalNucleoli","Mitoses")

for (col in dependents) {
  newCol=paste(col,"Interact",sep="")
  dataBinary[,newCol]=dataBinary[,col]*dataBinary$Missing
}

dataBinary[is.na(dataBinary$BareNuclei)==T, "BareNuclei"]=0

# sampling ID for training set
trainingSize=floor(0.75*nrow(rawData))
set.seed(1234)
trainRows=sample(x=rawData[,"ID"],size=trainingSize,replace=F)

library(kknn)
knnRun = function(data) {
  trainset=data[data$ID %in% trainRows==T,2:ncol(data)]
  testset=data[data$ID %in% trainRows==F,2:ncol(data)]
  modelTrain=train.kknn(Class~., trainset, kmax=15, 
                        kernel= c("rectangular", "triangular", "epanechnikov", 
                                  "gaussian", "rank", "optimal"))
  testResults=predict(modelTrain, testset)
  return (list("k"=modelTrain$best.parameters$k, 
               "kernel"=modelTrain$best.parameters$kernel,
               "accuracy"=sum(testResults==testset$Class)/nrow(testset)))
}

# use different data set and make comparison
r=data.frame(ncol=4,nrow=7)

r[1,1]="dataMode"
r[1,2]=knnRun(dataMode)$k
r[1,3]=knnRun(dataMode)$kernel
r[1,4]=knnRun(dataMode)$accuracy

r[2,1]="dataMean"
r[2,2]=knnRun(dataMean)$k
r[2,3]=knnRun(dataMean)$kernel
r[2,4]=knnRun(dataMean)$accuracy

r[3,1]="dataReg"
r[3,2]=knnRun(dataReg)$k
r[3,3]=knnRun(dataReg)$kernel
r[3,4]=knnRun(dataReg)$accuracy

r[4,1]="dataRegPerRetain"
r[4,2]=knnRun(dataRegPerRetain)$k
r[4,3]=knnRun(dataRegPerRetain)$kernel
r[4,4]=knnRun(dataRegPerRetain)$accuracy

r[5,1]="dataRegPerRound"
r[5,2]=knnRun(dataRegPerRound)$k
r[5,3]=knnRun(dataRegPerRound)$kernel
r[5,4]=knnRun(dataRegPerRound)$accuracy

r[6,1]="dataRemove"
r[6,2]=knnRun(dataRemove)$k
r[6,3]=knnRun(dataRemove)$kernel
r[6,4]=knnRun(dataRemove)$accuracy

r[7,1]="dataBinary"
r[7,2]=knnRun(dataBinary)$k
r[7,3]=knnRun(dataBinary)$kernel
r[7,4]=knnRun(dataBinary)$accuracy

colnames(r)=c("dataset", "best k", "best kernel", "accuracy")

r


groupcompare=data.frame(nrow=11, ncol=3)
var=c("ClumpThickness","UniformityofCellSize","UniformityofCellShape",
      "MarginalAdhesion","SingleEpithelialCellSize", "BareNuclei",
      "BlandChromatin","NormalNucleoli","Mitoses")
for (i in seq(1,length(var))){
  groupcompare[i,1]=var[i]
  groupcompare[i,2]=mean(rawData[is.na(rawData$BareNuclei)==T,var[i]])
  groupcompare[i,3]=mean(rawData[is.na(rawData$BareNuclei)==F,var[i]])
}

groupcompare[10,1]="Num.of benigh"
groupcompare[10,2]=sum(rawData[is.na(rawData$BareNuclei)==T,"Class"]=="benign")
groupcompare[10,3]=sum(rawData[is.na(rawData$BareNuclei)==F,"Class"]=="benign")

groupcompare[11,1]="Num.of malignant"
groupcompare[11,2]=sum(rawData[is.na(rawData$BareNuclei)==T,"Class"]=="malignant")
groupcompare[11,3]=sum(rawData[is.na(rawData$BareNuclei)==F,"Class"]=="malignant")

colnames(groupcompare)=c("variables", "missing-value group", "none-missing group")

groupcompare