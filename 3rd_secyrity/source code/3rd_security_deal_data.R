library(data.table)

# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("train.csv")
test <- fread("test.csv")
dim(train)
# [1] 409631049         6
head(train)
# file_id label                     api  tid return_value index
# 1:       0     0 GetSystemTimeAsFileTime 2644            0     0
# 2:       0     0 NtAllocateVirtualMemory 2644            0     1
# 3:       0     0     NtFreeVirtualMemory 2644            0     2
# 4:       0     0 NtAllocateVirtualMemory 2644            0     3
# 5:       0     0 NtAllocateVirtualMemory 2644            0     4
# 6:       0     0 NtAllocateVirtualMemory 2644            0     5
table(train$label)
# 0         1         2         3         4         5 
# 370157119   1369564   5964169   6108264    343644  25688289 
length(unique(train$api))
# 308
length(unique(train$file_id))
# 116624
length(unique(train$tid))
# 3578
length(unique(train$return_value))
# 2084069
apis <- unique(train$api)
# use number instead api string
for(i in 1:length(apis)){
  train[api==apis[i], api:=i]
  
}
write.csv(train,file = "train_api_digital.csv",
          row.names=FALSE,quote = FALSE)

for(i in 1:length(apis)){
  test[api==apis[i], api:=i]
  print(i)
}
write.csv(test,file = "test_api_digital.csv",
          row.names=FALSE,quote = FALSE)

file_ids <- unique(train$file_id)
for(i in 1:length(file_ids)){
  tmp <- train[file_id==file_ids[i],]
  if(length(unique(tmp$label)) > 1){
    print(i)
  }
}

tmp <- train[file_id==file_ids[31263],]
if(length(unique(tmp$tid)) > 1){
  print(i)
}
tmp$label
