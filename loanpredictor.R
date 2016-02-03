library("data.table")
library("ggplot2")

train= read.csv("input/loan/train.csv",header=TRUE,na.strings="BL")
test = read.csv("input/loan/test.csv",header=TRUE)

#test$Loan_Status<-data.frame(Loan_Status=rep("N", nrow(test)),test[,])

#print(test.Loan_Status)
test$Loan_Status<-'N'


data.combined = rbind(train,test)
#data.combined$Self_Employed<-unique(as.character(data.combined$Self_Employed
#))
#data.combined$Self_Employed <- as.character(data.combined$Self_Employed)
#data.missed <- data.combined[data.combined$Self_Employed==''] <- 'Jobles'

data.combined$missed <- data.combined[which(data.combined$Self_Employed==''),"Self_Employed"] <- 'No'

data.combined[data.combined$Loan_Status==''] <-'N'
#
#data.combined[which(data.combined$Loan_Status %in% "N"),]

#Factoring columns

data.combined$Self_Employed <- as.factor(data.combined$Self_Employed)

data.combined$Loan_Amount_Term <- as.factor(data.combined$Loan_Amount_Term)
data.combined$Credit_History<- as.factor(data.combined$Credit_History)

#str(data.combined)

df = subset(data.combined,(data.combined$Loan_Status=='Y'
& data.combined$Gender!='' 
& data.combined$Married!='' 
&data.combined$ApplicantIncome >= data.combined$CoapplicantIncome
))

#names(df)
table(df$Self_Employed,df$Loan_Status)

output = list(c(df["Loan_ID"],df["Loan_Status"]))
write.csv(file="output.csv",output)
ggplot(df, aes(x = c( df$Self_Employed), fill = factor(df$Loan_Status))) +
  geom_bar(binwidth = 5) +
  facet_wrap(~Self_Employed) +
  xlab("Applicant") +
  ylab("Total Count") +
    

  labs(fill = "LoanStatus") 





