#CUSUM

data = read.table("temps.txt", header=TRUE)  # import data
data[,1] = as.Date(data[,1], format="%d-%b") # change column 1's data type to Date
plot(data[,1], data[,2], 
     type="n", 
     xlab="date", 
     ylab="daily high temperature", 
     ylim=c(40,110))# build the plot
for (i in seq(2,21)){  # draw lines
  lines(data[,1], data[,i], col=i, lty=i-1)
}


cusum_model = function(data, column_num, T=-15, C=7){
  base = mean(data[1:31, column_num])  # July average
  s = 0  # cusum value
  for (i in seq(32,123)){  # traverse from Aug-1 to Oct-31
    new_s = s-(base-data[i, column_num]-C)
    if (new_s < 0){s = new_s}
    else {s = 0}
    if (s <= T){return (i)}  # return row number
  }
  return (0)
}


for (i in seq(2,21)){
  row_num = cusum_model(data,i)
  if (row_num != 0){
    cat(colnames(data)[i],":",format(data[row_num,1],format="%d-%b"),"\n")
  }
  else{print("Summer did not end...")}
}

plot(data[,1],data[,19],type="l",xlab="Date",ylab="Temperature of 2013")


data[,22] = data[,19]
data[46:48,22] = mean(data[c(41,42,43,44,45,49,50,51,52,53),22])
colnames(data)[22] = "2013NEW"
row_num = cusum_model(data,22)
if (row_num != 0){
  cat(colnames(data)[22],":",format(data[row_num,1],format="%d-%b"),"\n")
}


data[,19] = data[,22]  # replace 2013 data with the new data we derive above
data = data[,-22]  # delete the extra column

summer_average = matrix(nrow=20, ncol=2)  # store average temperature data
summer_average[,1] = seq(1996,2015)
for (i in seq(2,21)){
  row_num = cusum_model(data,i)  # row number that summer ends
  average_value = mean(data[1:row_num-1,i])  # average temperature during summer
  summer_average[i-1,2] = average_value
}
cat("average summer daily-high-temperature from 1996 to 2015:","\n");summer_average


base = mean(summer_average[1:3,2])  # baseline, Year 1996 to 1998
s = 0  # cusum value
for (i in seq(4,20)){  # traverse from Year 1999 to 2015
  new_s = s+(summer_average[i,2]-base-2.5)
  if (new_s > 0){s = new_s}
  else {s = 0}
  if (s >= 5){print(summer_average[i,1]);break}  # print year
}
if (s<5){print("Atlanta's summer climate has not gotten warmer")}