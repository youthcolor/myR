library(data.table)
#library(sampling)
#library(caret)
# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("file_id_api_seq_label_N_0.1.csv")

file_ids <- unique(train$file_id)
train2 <- fread("train_api_digital.csv")
train2_select <- train2[file_id %in% file_ids]
head(train2_select)
train2_select_0 <- train2_select[label==0][,.N,by=.(api)][order(-N)]
head(train2_select_0)
train2_select_0_api <- head(train2_select_0,20)$api

train2_select_1 <- train2_select[label==1][,.N,by=.(api)][order(-N)]
head(train2_select_1)
train2_select_1_api <- head(train2_select_1,20)$api

train2_select_2 <- train2_select[label==2][,.N,by=.(api)][order(-N)]
head(train2_select_2)
train2_select_2_api <- head(train2_select_2,20)$api

train2_select_3 <- train2_select[label==3][,.N,by=.(api)][order(-N)]
head(train2_select_3)
train2_select_3_api <- head(train2_select_3,20)$api

train2_select_4 <- train2_select[label==4][,.N,by=.(api)][order(-N)]
head(train2_select_4)
train2_select_4_api <- head(train2_select_4,20)$api

train2_select_5 <- train2_select[label==5][,.N,by=.(api)][order(-N)]
head(train2_select_5)
train2_select_5_api <- head(train2_select_5,20)$api
select_apis <- c(train2_select_0_api,train2_select_1_api,train2_select_2_api,
                 train2_select_3_api,train2_select_4_api,train2_select_5_api)
select_apis <- unique(select_apis)
# [1]  22  15   6  74 229  12  98  23  68  55  31
train2_select_N <- train2_select[,.N,by=.(file_id,api)]
df <- data.table()
for(i in 1:length(file_ids)){
  temp <- train2_select_N[file_id==file_ids[i]]
  df <- rbind(df,data.table(file_id=file_ids[i]),fill=TRUE)
  #df[i,file_id:=file_ids[i]]
  for(j in 1:length(select_apis)){
    temp2 <- temp[api==select_apis[j]]
    if(nrow(temp2)>0){
      df[i,paste("apiapi",j,sep = "_"):=temp2$N]
    }else{
      df[i,paste("apiapi",j,sep = "_"):=0]
    }
  }
  print(i)
}
summary(df)
write.csv(df,"file_id_apiapi_41_select_N.csv",row.names = F,quote = F)

file_label_select <- train2_select[,.N,by=.(file_id,label)][,N:=NULL]
write.csv(file_label_select,"file_id_label_select.csv",row.names = F,quote = F)


# test data
rm(train2,train2_select)
test <- fread("test_api_digital.csv")[,.N,by=.(file_id,api)]
file_ids <- unique(test$file_id)
df <- data.table()
for(i in 1:length(file_ids)){
  temp <- test[file_id==file_ids[i]]
  df <- rbind(df,data.table(file_id=file_ids[i]),fill=TRUE)
  #df[i,file_id:=file_ids[i]]
  for(j in 1:length(select_apis)){
    temp2 <- temp[api==select_apis[j]]
    if(nrow(temp2)>0){
      df[i,paste("apiapi",j,sep = "_"):=temp2$N]
    }else{
      df[i,paste("apiapi",j,sep = "_"):=0]
    }
  }
  print(i)
}
write.csv(df,"file_id_apiapi_41_select_N_test.csv",row.names = F,quote = F)
