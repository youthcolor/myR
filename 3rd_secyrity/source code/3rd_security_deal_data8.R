library(data.table)
#library(sampling)
#library(caret)
# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("file_id_api_seq_label_N_0.1.csv")


api_seqs <- unique(train$api_seq)
c0 <- train[label==0,] 
c1 <- train[label==1,] 
c2 <- train[label==2,] 
c3 <- train[label==3,] 
c4 <- train[label==4,] 
c5 <- train[label==5,] 
rm(train)
#result <- data.frame()
n0 <- c()
n1 <- c()
n2 <- c()
n3 <- c()
n4 <- c()
n5 <- c()
for(i in 1:length(api_seqs)){
  n0[i] <- length(unique(c0[api_seq==api_seqs[i],]$file_id))
  n1[i] <- length(unique(c1[api_seq==api_seqs[i],]$file_id))
  n2[i] <- length(unique(c2[api_seq==api_seqs[i],]$file_id))
  n3[i] <- length(unique(c3[api_seq==api_seqs[i],]$file_id))
  n4[i] <- length(unique(c4[api_seq==api_seqs[i],]$file_id))
  n5[i] <- length(unique(c5[api_seq==api_seqs[i],]$file_id))
  #result <- rbind(result,data.frame(api_seq=api_seqs[i],n0=n0,n1=n1,n2=n2,n3=n3,n4=n4,n5=n5))
  print(i)
}
result <- data.frame(api_seq=api_seqs,n0=n0,n1=n1,n2=n2,n3=n3,n4=n4,n5=n5)
write.csv(result,"file_id_api_seq_label_N_0.1_statistic.csv",row.names = F,quote = F)
