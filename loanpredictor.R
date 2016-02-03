df = read.csv("train.csv")

df = subset(df,df["Loan_Status"]=='Y')

output = list(c(df["Loan_ID"],df["Loan_Status"]))
 
 write.csv(file="output.csv",output) 
