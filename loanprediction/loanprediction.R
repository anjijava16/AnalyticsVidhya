library("data.table")
library("ggplot2")
library("randomForest")
library(caret)
library(randomForest)


train= read.csv("input/loan/train.csv",header=TRUE)
test = read.csv("input/loan/test.csv",header=TRUE)

fmt_test<-data.frame(Loan_Status=rep("N" ,nrow(test)),test[,]); 

combined<-rbind(train,fmt_test)


result<-combined[1:367,]
print( nrow(result))
 
result[which(result$ApplicantIncome>result$CoapplicantIncome),"Loan_Status"]<-'Y'

 

output<-list(c(result["Loan_ID"],result["Loan_Status"]))


write.csv(file="output.csv",output, row.names=F)
 
