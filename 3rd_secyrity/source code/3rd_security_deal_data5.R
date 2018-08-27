library(data.table)

# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("file_id_api_seq_label.csv")
head(train)
train2 <- train[,.N,by=.(file_id,api_seq)]
dim(train2)
head(train2,10)
id_label <- train[,.N,by=.(file_id,label)]
head(id_label)
write.csv(id_label,"id_label_N.csv",row.names = F,quote = F)
combin <- train2[id_label,on="file_id"]
head(combin)
colnames(combin) <- c("file_id","api_seq","api_seq_N","label","label_N")
write.csv(combin,"id_label_api_seq_Ns.csv",row.names = F,quote = F)

combin_label_0 <- combin[label==0,][order(-api_seq_N)]
head(combin_label_0,10)

combin_label_1 <- combin[label==1,][order(-api_seq_N)]
combin_label_1_by <- combin_label_1[,.(sum(api_seq_N),.N),by=.(api_seq)]
head(combin_label_1,20)
head(combin_label_1_by,20)
