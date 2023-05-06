#install package to import Excel xlsx
#count NA in each column
install.packages("readxl") 
library("readxl")
df<-read_excel("prepared-data.xlsx")
df
print(sapply(df, function(x) sum(is.na(x)))) 

#model-based imputation for missing data
i<-1
j<-1
for (i in 1:nrow(df)){
  for (j in 1:ncol(df)){
    if (is.na(df[i,j])==TRUE){
      fit_data<-df[(i-2):(i+2),c(2,j)]
      model<-lm(unlist(df[(i-2):(i+2),j])~unlist(df[(i-2):(i+2),"Years"]),fit_data,na.action=na.exclude)
      new_data<-df[(i-2):(i+2),2]
      predicted_data<-predict(model,new_data)
      df[i,j]<-predicted_data[3]
    }else if(i==40){
      break
    }else{
      i=i+1
      j=j+1
    }
  }
}

#round to 1 decimal place
df[3:15]<-round(df[3:15], 1) 
write.csv(df, file="clean-data.csv",row.names=FALSE)
