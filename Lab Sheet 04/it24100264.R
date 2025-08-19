setwd("C:\\Users\\it24100264\\Desktop\\it24100264")
#Q1
branch_data <-read.table("Exercise.txt",header=TRUE,sep=",")

#Q2
fix(branch_data)
str(branch_data)
attach(branch_data)

#Q3
#Obtain boxplot for sales_X1
boxplot(branch_data$Sales_X1,main="Box plot for sales",outline=TRUE,outpch=8,horizontal = TRUE)
#Q4
#five numbery summery
summary(Advertising_X2)
IQR(Advertising_X2)

#Q5
get.outliers<-function(z){
  q1<-quantile(z)[2]
  q3<-quantile(z)[4]
  iqr<-q3-q1
  
  ub<-q3+1.5*iqr
  lb<-q1-1.5*iqr
  
  print(paste("upper Bound=",ub))
  print(paste("Lower Bound=",lb))
  print(paste("Outliers:",paste(sort(z[z<lb | z>ub]),collapse = ",")))
        
}
#check for outliers in years variables
get.outliers(Years_X3)