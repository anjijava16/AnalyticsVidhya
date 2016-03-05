
library("ggplot2")



printGraph<- function(Approved,Gender,Loan_Status){
  ggplot(train, aes(Gender, Loan_Status)) +
    geom_bar( stat = "identity") +
    theme(axis.text.x = element_text(angle = 70, vjust =0.10 , color = "navy")) +
    xlab("Gender") + ylab("Loan_Status")+ggtitle("Gender vs Loan_Status")
  
  ggplot(train, aes(LoanAmount, Loan_Status)) +
    geom_bar( stat = "identity") +
    theme(axis.text.x = element_text(angle = 70, vjust =0.10 , color = "navy")) +
    xlab("LoanAmount") + ylab("Loan_Status")+ggtitle("LoanAmount vs Loan_Status")
  
  ggplot(train, aes(Loan_Amount_Term, Loan_Status)) +
    geom_bar( stat = "identity") +
    theme(axis.text.x = element_text(angle = 70, vjust =0.10 , color = "navy")) +
    xlab("Loan_Amount_Term") + ylab("Loan_Status")+ggtitle("Loan_Amount_Term vs Loan_Status")
  
  
   
  
}
format<-function(df){
  
  #LoanAmount  Loan_Amount_Term    Credit_History
  #3.Dependents
  df$Dependents<-as.character(df$Dependents)
  df[df$Dependents=='','Dependents'] <- -1
  df[df$Dependents=='3+','Dependents'] <- '3'
  df$Dependents<-as.factor(df$Dependents)
  
  library(plyr)
  #df[ is.na(df)]<- -1
  
  #5.LoanAmount
  
  #write.csv(file="combined.csv",combined, row.names=F)
  
    return (df)
  
}

calculateMetrics<-function(formula,df){
  library(Metrics)
  library(dplyr)
  
  linear_model <- lm(formula , data =df)
  result<-rmse(train$Loan_Status, exp(linear_model$fitted.values))
 
  
}

formatcategorialVar<-function(df){
  library(plyr)
  
  #Gender
  df$Gender<-as.character(df$Gender)
  df[which(df$Gender==''),'Gender']<-'NS'
  df$Gender<-as.factor(df$Gender)
  
  #Self Employeed
  df$Self_Employed<-as.character(df$Self_Employed)
  df[which(df$Self_Employed==''),'Self_Employed']<-'NS'
  df$Self_Employed<-as.factor(df$Self_Employed)
  
  #Married
  #df$Married<-as.character(df$Married)
  #df[which(df$Married==''),'Married']<-'NS'
  #df$Married<-as.factor(df$Married)
  
  return(df)
}

imputeMissingValue<-function(df){
  
  #KNN computes only for the numeric value not for the Char
  #use KNN as such since 2 function similar name is there
  #kNN imputation is correct function name
  library(VIM)
  
 df<- kNN(df,variable=c('Gender','Self_Employed','Married',
                    'Credit_History','Loan_Amount_Term',
                    'LoanAmount','Dependents'))
  
  return(df)
}

library(data.table)

#combinedTable<-data.table(combined)


#############################3. Analyze data using train generated.
train= read.csv("train.csv",header=TRUE)
test = read.csv("test.csv",header=TRUE)
fmt_test<-data.frame(Loan_Status=rep("N" ,nrow(test)),test[,]); 
combined<-rbind(fmt_test,train)

###train

train<-combined[368:981,]
test<-combined[1:367,]



###########train data formatting
train<-imputeMissingValue(train)
colSums(is.na(train))

train<-formatGender(train)
train<-format(train)
train<-formatcategorialVar(train)




X_train<-train
X_train<-subset(X_train, !(X_train$Gender=='NS'))
X_train<-subset(X_train, !X_train$Self_Employed=='NS')
X_train<-subset(X_train, !X_train$Married=='NS')
X_train<-subset(X_train, !X_train$Dependents==-1)


#X_train<-subset(X_train, !(X_train$Credit_History==-1) )
#X_train<-subset(X_train, !(X_train$Loan_Amount_Term==-1))
#X_train<-subset(X_train, !(X_train$LoanAmount==-1))






write.csv(file="fmttrain.csv",X_train, row.names=F)

#################test
test<-imputeMissingValue(test)
test<-formatGender(test)
test<-format(test)
test<-formatcategorialVar(test)

colSums(is.na(test))
X_test<-test
X_test<-subset(X_test, !(X_test$Gender=='NS'))
X_test<-subset(X_test, !X_test$Self_Employed=='NS')
X_test<-subset(X_test, !X_test$Married=='NS')
X_test<-subset(X_test, !X_test$Dependents==-1)

#X_test<-subset(X_test, !(X_test$Credit_History==-1) )
#X_test<-subset(X_test, !(X_test$Loan_Amount_Term==-1))
#X_test<-subset(X_test, !(X_test$LoanAmount==-1))



# colSums(is.na(test))

write.csv(file="fmttest.csv",X_test, row.names=F)


 

 #############################4. predict the loan_status

library(randomForest)
set.seed(615)
formula<-Loan_Status ~ Gender+Married+Dependents+Education+ 
  Self_Employed+ApplicantIncome +CoapplicantIncome+LoanAmount+
  Loan_Amount_Term+Credit_History+Property_Area





fit <- randomForest(formula, data=X_train,importance=TRUE, ntree=38)


print(nrow(X_test))
print(nrow(X_train))
#colSums(is.na(test))
pred=predict(fit,X_test)
X_test$Loan_Status=pred



library(plyr)
missing_test<-test
result<-join(X_test,test,type="right",by="Loan_ID")


 
nrow(X_test)
nrow(result)
nrow(test)-nrow(X_test)
#result[is.na(result$Loan_Status),'Loan_Status']<-'Y'
#############################6. Generate Output

write.csv(file="output.csv",c(result['Loan_ID'],result['Loan_Status']), row.names=F)


