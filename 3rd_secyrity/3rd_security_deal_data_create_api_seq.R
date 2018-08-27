library(data.table)

# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("train_api_digital.csv")
head(train)
train[,tid:=NULL][,return_value:=NULL][,index:=NULL]
#train[,c("label","tid","return_value","index"):=NULL]
#train <- train[order(file_id,index)]
#write.csv(train,file = "file_id_api_index.csv",row.names=FALSE,quote=FALSE)

# train <- cbind(train,api_1=c(0,head(train$api,-1)),
#                api_2=c(0,0,head(train$api,-2)),
#                file_id_1=c(0,head(train$file_id,-1)),
#                file_id_2=c(0,0,head(train$file_id,-2)))
# train[,api_seq:=ifelse(file_id_1==file_id,ifelse(file_id_2==file_id,
#                                                  paste(api,api_1,api_2),
#                                                  paste(api,api_1,0)),
#                        paste(api,0,0))]

train <- cbind(train,api_1=c(0,head(train$api,-1)),
               api_2=c(0,0,head(train$api,-2)),
               api_3=c(0,0,0,head(train$api,-3)),
               file_id_1=c(0,head(train$file_id,-1)),
               file_id_2=c(0,0,head(train$file_id,-2)),
               file_id_3=c(0,0,0,head(train$file_id,-3)))
train[,api_seq:=ifelse(file_id_1==file_id,
                       ifelse(file_id_2==file_id,
                              ifelse(file_id_3==file_id,
                                     paste(api,api_1,api_2,api_3),
                                     paste(api,api_1,api_2,0)),
                              paste(api,api_1,0,0)),
                       paste(api,0,0,0))]

train[,c("api","api_1","api_2","api_3","file_id_1","file_id_2","file_id_3"):=NULL]
write.csv(train,file = "file_id_api_seq_4_train.csv",row.names=FALSE,quote=FALSE)
