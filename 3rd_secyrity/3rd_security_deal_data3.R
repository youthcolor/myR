library(data.table)

# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("file_id_3_api_3.csv")
head(train)
#train[,c("label","tid","return_value","index"):=NULL]
#train <- train[order(file_id,index)]
#write.csv(train,file = "file_id_api_index.csv",row.names=FALSE,quote=FALSE)

train[,api_seq:=ifelse(file_id_1==file_id,ifelse(file_id_2==file_id,
                                                 paste(api,api_1,api_2),
                                                 paste(api,api_1,0)),
                       paste(api,0,0))]
train[,c("api","api_1","api_2","file_id_1","file_id_2"):=NULL]
write.csv(train,file = "file_id_api_seq.csv",row.names=FALSE,quote=FALSE)
