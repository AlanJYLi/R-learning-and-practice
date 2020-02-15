# PCA
data = read.table("uscrime.txt", header=TRUE)  # import data
crime_features = data[,-16]
pca_model = prcomp(crime_features, scale.=TRUE)
summary(pca_model)

variance_proprtion = c(0.4013,0.1868,0.1337,0.07748,0.06389,
                       0.03688,0.02145,0.02049,0.01568,0.01333,
                       0.01171,0.00855,0.00462,0.0039,0.00031)
plot(variance_proprtion, type='l')

pc_seven = pca_model$x[,1:7] # get the PC1 to PC7 values for each data point 
crime_pc = data.frame((cbind(pc_seven, data[,16])))
colnames(crime_pc)[8] = 'Crime'
model_1 = lm(Crime~., data=crime_pc)
summary(model_1)

loading = pca_model$rotation[,1:7] # loading matrix from PCA model
coe_pc = model_1$coefficients[-1] # coefficients of regression Y~PCs
coe_origin = loading%*%coe_pc # coefficients of regression Y~scaled original features
coe_origin
intercept_origin = model_1$coefficients[1]
intercept_origin

sd = pca_model$scale
coe_unscale = coe_origin / sd
coe_unscale
intercept_unscale = intercept_origin - sum(pca_model$center * coe_unscale)
intercept_unscale

new_data_point = c(14, 0, 10, 12, 15.5, 
                   0.64, 94, 150, 1.1, 0.12, 
                   3.6, 3200, 20.1, 0.04, 39)
new_data_point%*%coe_unscale+intercept_unscale