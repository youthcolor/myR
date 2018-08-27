library(data.table)
library(sampling)
library(caret)
# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("id_label_N.csv")
head(train)

# boxplot(N~label,data = train)
train$label <- as.factor(train$label)

table(train$label)
train_0 <- train[label==0]
index <- createDataPartition(y=train_0$label, p=0.1, list=F) 
train_0 <- train_0[index,]
table(train$label)
table(train_0$label)
train.new <- rbind(train_0,train[label==1],train[label==2],
                   train[label==3],train[label==4],train[label==5])
table(train.new$label)
head(train.new)
ids <- train.new$file_id
rm(train)
train <- fread("file_id_api_seq_label_N.csv")
head(train)
train.select <- train[file_id %in% ids,]
write.csv(train.select,"file_id_api_seq_label_N_0.1.csv",row.names = F,quote = F)
