library(data.table)

# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("train_api_digital.csv")
api_seq <- fread("file_id_api_seq.csv")
file_id_api_seq_label <- data.frame(api_seq,label=train$label)
write.csv(file_id_api_seq_label,file = "file_id_api_seq_label.csv",
          row.names=FALSE,quote=FALSE)
#train <- train[order(file_id,index)]
#train[,c("label","tid","return_value","index"):=NULL]
#train <- train[order(file_id,index)]


