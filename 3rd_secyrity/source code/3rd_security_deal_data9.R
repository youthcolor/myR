library(data.table)
#library(sampling)
#library(caret)
# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("file_id_api_seq_label_N_0.1.csv")
cc <- length(unique(train$file_id))
c0 <- length(unique(train[label==0,]$file_id))
c1 <- length(unique(train[label==1,]$file_id))
c2 <- length(unique(train[label==2,]$file_id))
c3 <- length(unique(train[label==3,]$file_id))
c4 <- length(unique(train[label==4,]$file_id))
c5 <- length(unique(train[label==5,]$file_id))
rm(train)

train_statistic <- fread("file_id_api_seq_label_N_0.1_statistic.csv")

getEntropy <- function(n0,n1,n2,n3,n4,n5){
  result = 0
  if(n0>0){
    result <- (n0/c0)*log((n0/c0)/(((n0+n1+n2+n3+n4+n5)/cc)*(c0/cc))) +
      ((c0-n0)/c0)*log(((c0-n0)/c0)/(((cc-n0-n1-n2-n3-n4-n5)/cc)*(c0/cc)))
  }
  if(n1>0){
    result <- (n1/c1)*log((n1/c1)/(((n0+n1+n2+n3+n4+n5)/cc)*(c1/cc))) +
      ((c1-n1)/c1)*log(((c1-n1)/c1)/(((cc-n0-n1-n2-n3-n4-n5)/cc)*(c1/cc)))
  }
  if(n2>0){
    result <- (n2/c2)*log((n2/c2)/(((n0+n1+n2+n3+n4+n5)/cc)*(c2/cc))) +
      ((c2-n2)/c2)*log(((c2-n2)/c2)/(((cc-n0-n1-n2-n3-n4-n5)/cc)*(c2/cc))) 
  }
  if(n3>0){
    result <- (n3/c3)*log((n3/c3)/(((n0+n1+n2+n3+n4+n5)/cc)*(c3/cc))) +
      ((c3-n3)/c3)*log(((c3-n3)/c3)/(((cc-n0-n1-n2-n3-n4-n5)/cc)*(c3/cc)))
  }
  if(n4>0){
    result <- (n4/c4)*log((n4/c4)/(((n0+n1+n2+n3+n4+n5)/cc)*(c4/cc))) +
      ((c4-n4)/c4)*log(((c4-n4)/c4)/(((cc-n0-n1-n2-n3-n4-n5)/cc)*(c4/cc)))
  }
  if(n5>0){
    result <- (n5/c5)*log((n5/c5)/(((n0+n1+n2+n3+n4+n5)/cc)*(c5/cc))) +
      ((c5-n5)/c5)*log(((c5-n5)/c5)/(((cc-n0-n1-n2-n3-n4-n5)/cc)*(c5/cc)))
  }
  
  return(result)
}

for(i in 1:nrow(train_statistic)){
  train_statistic[i,entropy:=getEntropy(n0,n1,n2,n3,n4,n5)]
  print(i)
}


write.csv(train_statistic,"file_id_api_seq_label_N_0.1_entropy.csv",row.names = F,quote = F)

summary(train_statistic$entropy)
nrow(train_statistic[entropy>5.9])
api_select <- train_statistic[entropy>5.9]$api_seq
train <- fread("file_id_api_seq_label_N_0.1.csv")
file_ids <- unique(train$file_id)
df <- data.table()
for(i in 1:length(file_ids)){
  temp <- train[file_id==file_ids[i]]
  df <- rbind(df,data.table(file_id=file_ids[i]),fill=TRUE)
  #df[i,file_id:=file_ids[i]]
  for(j in 1:length(api_select)){
    temp2 <- temp[api_seq==api_select[j]]
    if(nrow(temp2)>0){
      df[i,paste("api",j,sep = "_"):=temp2$N]
    }else{
      df[i,paste("api",j,sep = "_"):=0]
    }
  }
  print(i)
}
summary(df)
write.csv(df,"file_id_api_102_select_N.csv",row.names = F,quote = F)
