# Cross Validation for KNN
library("kknn")
set.seed(1234)
data = read.table("credit_card_data-headers.txt", header=TRUE)  # import data
data[,11] = as.factor(data[,11])  # change the dependent variable into norminal
index = sample(x=2, size=nrow(data), replace=TRUE, prob=c(0.8,0.2))
# split data for cross validation and testing
data_cross_validation = data[index==1,]
data_testing = data[index==2,]

cross_validation_knn = function(dis_type, kernel_type){
  # output 10-fold cross validation results from k=5 to k=15, 
  # in which distance and kernel as parameters
  set.seed(1234)
  results = vector()
  for (i in seq(5,15)){
    cvknn_model = cv.kknn(R1~., data_cross_validation, kcv=10, 
                          distance=dis_type, kernel=kernel_type, 
                          k=i, scale=TRUE)
    # use kknn library's cross validation function cv.kknn
    results[i-4] = round(cvknn_model[[2]]*100,3)
    # average accuracy(%) in 10-fold cross validation for a certain model
  }
  return (results)
}

setting_combination = matrix(nrow=6,ncol=2)  # store the setting parameters
setting_combination[,1] = c(1,2,1,2,1,2)  # distance parameter
setting_combination[,2] = c("rectangular","rectangular",
                            "triangular","triangular",
                            "optimal","optimal")  # kernel parameter

compare_table = matrix(nrow=11, ncol=7)  # store the accuracy value of different models
compare_table[,1] = c(5,6,7,8,9,10,11,12,13,14,15)  # k values for knn model
colnames(compare_table) = c("k","Euc.dis_rec","Min.dis_rec",
                            "Euc.dis_tri","Min.dis_tri",
                            "Euc.dis_opt","Min.dis_opt")

for (i in seq(1,nrow(setting_combination))){
  # traverse different distance and kernel settings
  dis_type = setting_combination[i,1]
  kernel_type = setting_combination[i,2]
  compare_table[,i+1] = cross_validation_knn(dis_type, kernel_type)
}

cat("Accuracy for Different Models\n"); compare_table
max_location = which(compare_table==max(compare_table), arr.ind=TRUE)
# find the row number and column num of maximum accuracy
cat("The maximum accuracy is at: row",max_location[1,1],", col",max_location[1,2])


selected_model = kknn(R1~.,data_cross_validation, data_testing, 
                      distance=2, kernel="rectangular", k=8,scale=TRUE)
fitting_result = table(data_testing$R1, fitted(selected_model))
# counting table for predicting category against real category
matching_percentage = round(sum(fitting_result[1,1],fitting_result[2,2])
                            /sum(fitting_result)*100,3)
matching_percentage


# Cross Validation for SVM
library("kernlab")
set.seed(1234)
data = data.matrix(read.table("credit_card_data-headers.txt", header=TRUE))
# import data and change to matrix
index = sample(x=2, size=nrow(data), replace=TRUE, prob=c(0.8,0.2))
# split data for cross validation and testing
data_cross_validation = data[index==1,]
data_testing = data[index==2,]

cross_validation_svm = function(kernel_type, C_values){
  # output 10-fold cross validation results for each SVM model
  set.seed(1234)
  cvsvm_model = ksvm(data_cross_validation[,1:10],data_cross_validation[,11],
                     type="C-svc", kernel=kernel_type, C=C_values, 
                     cross=10, scaled=TRUE)
  return (round((1-cvsvm_model@cross)*100,3))  # accuracy in percentage
}

C_set = c(1,100,10000)  # C values
kernel_set = c("rbfdot", "polydot", "vanilladot", "tanhdot", "anovadot")  # kernels

compare_table = matrix(nrow=3, ncol=6)  # store the accuracy value of different models
compare_table[,1] = C_set  # C values for ksvm model
colnames(compare_table) = c('C',"rbfdot","polydot","vanilladot","tanhdot","anovadot")

for (i in seq(1,length(C_set))){
  for (j in seq(1,length(kernel_set))){
    compare_table[i,j+1] = cross_validation_svm(kernel_set[j], C_set[i])
  }
}

compare_table

selected_model = ksvm(data_cross_validation[,1:10],data_cross_validation[,11],
                      type="C-svc", kernel="vanilladot", C=100, scaled=TRUE)

pred = predict(selected_model, data_testing[,1:10])
matching_percentage = round(sum(pred==data_testing[,11])/nrow(data_testing)*100,3)
x <- selected_model@xmatrix[[1]]
coe <- selected_model@coef[[1]]
a <- colSums(x*coe)
a0 <- -selected_model@b
cat("Accuracy:",matching_percentage,"\n")
cat("classifier coefficients:\n");a
cat("classifier intercept:\n");a0



# Model training, validation and test
# KNN
library("kknn")
set.seed(1234)
data = read.table("credit_card_data-headers.txt", header=TRUE)  # import data
data[,11] = as.factor(data[,11])  # change the dependent variable into norminal
index = sample(x=3, size=nrow(data), replace=TRUE, prob=c(0.6,0.2,0.2))
# split data for training, validation and testing
data_training = data[index==1,]
data_validation = data[index==2,]
data_testing = data[index==3,]

validation_knn = function(dis_type, kernel_type){
  # output KNN model validation results, in which kernel and C as parameters
  results = vector()
  for (i in seq(5,15)){
    knn_model = kknn(R1~., data_training, data_validation, 
                     distance=dis_type, kernel=kernel_type, k=i, scale=TRUE)
    fitting_result = table(data_validation$R1, fitted(knn_model))
    # counting table for predicting category against real category
    results[i-4] = round(sum(fitting_result[1,1],fitting_result[2,2])/
                           sum(fitting_result)*100,3)
    # accuracy(%) for validation for different k values
  }
  return (results)
}

setting_combination = matrix(nrow=6,ncol=2)  # store the setting parameters
setting_combination[,1] = c(1,2,1,2,1,2)  # distance parameter
setting_combination[,2] = c("rectangular","rectangular",
                            "triangular","triangular",
                            "optimal","optimal")  # kernel parameter

compare_table = matrix(nrow=11, ncol=7)  # store the accuracy value of different models
compare_table[,1] = c(5,6,7,8,9,10,11,12,13,14,15)  # k values for knn model
colnames(compare_table) = c("k","Euc.dis_rec","Min.dis_rec",
                            "Euc.dis_tri","Min.dis_tri",
                            "Euc.dis_opt","Min.dis_opt")

for (i in seq(1,nrow(setting_combination))){
  # traverse different distance and kernel settings
  dis_type = setting_combination[i,1]
  kernel_type = setting_combination[i,2]
  compare_table[,i+1] = validation_knn(dis_type, kernel_type)
}

cat("Accuracy for Different Models\n");compare_table
max_location = which(compare_table==max(compare_table), arr.ind=TRUE)
# find the row number and column num of maximum accuracy
cat("The maximum accuracy is at: row",max_location[1,1],", col",max_location[1,2])

selected_model = kknn(R1~.,data_training, data_testing, 
                      distance=1, kernel="optimal", k=10,scale=TRUE)
fitting_result = table(data_testing$R1, fitted(selected_model)) 
# counting table for predicting category against real category
matching_percentage = round(sum(fitting_result[1,1],fitting_result[2,2])/
                              sum(fitting_result)*100,3)
matching_percentage


# SVM
library("kernlab")
set.seed(1234)
data = data.matrix(read.table("credit_card_data-headers.txt", header=TRUE))  
# import data and change to matrix
index = sample(x=3, size=nrow(data), replace=TRUE, prob=c(0.6,0.2,0.2))  
# split data for training, validation and testing
data_training = data[index==1,]
data_validation = data[index==2,]
data_testing = data[index==3,]

validation_svm = function(kernel_type, C_values){  
  # output SVM model validation results, in which kernel and C as parameters
  svm_model = ksvm(data_training[,1:10], data_training[,11], 
                   type="C-svc", kernel=kernel_type, C=C_values, scaled=TRUE)
  pred = predict(svm_model, data_validation[,1:10])
  return (round(sum(pred==data_validation[,11])/nrow(data_validation)*100,3))  
  # return the validating results
}

C_set = c(1,100,10000)  # C values
kernel_set = c("rbfdot","polydot","vanilladot","tanhdot","anovadot")  # kernels

compare_table = matrix(nrow=3, ncol=6)  # store the accuracy value of different models
compare_table[,1] = C_set  # C values for ksvm model
colnames(compare_table) = c('C',"rbfdot", "polydot", 
                            "vanilladot", "tanhdot", "anovadot")

for (i in seq(1,length(C_set))){
  for (j in seq(1,length(kernel_set))){
    compare_table[i,j+1] = validation_svm(kernel_set[j], C_set[i])
  }
}

compare_table

selected_model = ksvm(data_training[,1:10], data_training[,11], 
                      type="C-svc", kernel="tanhdot", C=100, scaled=TRUE)
pred = predict(selected_model, data_testing[,1:10])
matching_percentage = round(sum(pred==data_testing[,11])/nrow(data_testing)*100,3)
x <- selected_model@xmatrix[[1]]
coe <- selected_model@coef[[1]]
a <- colSums(x*coe)
a0 <- -selected_model@b

cat("Accuracy:",matching_percentage,"\n")
cat("classifier coefficients:\n");a
cat("classifier intercept:\n");a0


