library(data.table)
library(DMwR) 
#library(sampling)
#library(caret)
library(h2o)
library(caret)
h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train1 <- fread("file_id_apiapi_49_select_N_train.csv")
train2 <- fread("file_id_label_select_train.csv")
train3 <- fread("file_id_rv_61_select_N_train.csv")
train4 <- fread("file_id_api_seq_66_select_N_train_0.csv")

train11 <- fread("file_id_apiapi_93_select_N_train.csv")
train33 <- fread("file_id_rv_116_select_N_train.csv")
train44 <- fread("file_id_api_seq_191_select_N_train.csv")
names(train11) <- paste(names(train11),"del",sep = "_")
names(train33) <- paste(names(train33),"del",sep = "_")
names(train44) <- paste(names(train44),"del",sep = "_")
names(train11)[1] <- "file_id"
names(train33)[1] <- "file_id"
names(train44)[1] <- "file_id"
# write.csv(train,"train_115.csv",row.names = F,quote = F)

train <- train1[train3,on="file_id"][train4,on="file_id"][train2,on="file_id"][train11,on="file_id"][train33,on="file_id"][train44,on="file_id"]
rm(train1,train2,train3,train4,train11,train33,train44)
train[,file_id:=NULL]
table(train$label)
train$label <- as.factor(train$label)
trainSplit <- SMOTE(label~.,train,perc.over=500,perc.under = 6150)
table(trainSplit$label)

trainSplit4 <- SMOTE(label~.,train,perc.over=6000,perc.under = 516)
table(trainSplit4$label)
trainSplit1 <- SMOTE(label~.,trainSplit4,perc.over=1000,perc.under = 670)
table(trainSplit1$label)
trainSplit3 <- SMOTE(label~.,trainSplit1,perc.over=500,perc.under = 800)
table(trainSplit3$label)
trainSplit2 <- SMOTE(label~.,trainSplit3,perc.over=390,perc.under = 1070)
table(trainSplit2$label)
rm(trainSplit,trainSplit1,trainSplit3,trainSplit4,train)
# library(ROSE)
# data.rose <- ROSE(label ~ ., data =train[label==0|label==1], seed = 1)$data
# table(data.rose$label)



control = trainControl(method = "repeatedcv",number = 5,repeats = 3)
library(rpart)
library(C50)
model = train(label~.,data = train,method = "rpart",
              trControl = control)
importance = varImp(model,scale = FALSE)
plot(importance)
X <- rownames(head(importance$importance,25))

train.h2o <- as.h2o(trainSplit)

aml <- h2o.automl(x=X,y = "label", training_frame = train.h2o, 
                  max_runtime_secs = 3600,exclude_algos=c("GLM", "DeepLearning","DRF"),
                  sort_metric="logloss")
aml1 <- h2o.getModel("GBM_grid_0_AutoML_20180821_220833_model_2")
X <- head(aml1@model$variable_importances,25)$variable
h2o.removeAll()

test1 <- fread("file_id_apiapi_49_select_N_test.csv")
test2 <- fread("file_id_label_select_test.csv")
test3 <- fread("file_id_rv_61_select_N_test.csv")
test4 <- fread("file_id_api_seq_66_select_N_test_0.csv")
test11 <- fread("file_id_apiapi_93_select_N_test.csv")
test33 <- fread("file_id_rv_116_select_N_test.csv")
test44 <- fread("file_id_api_seq_191_select_N_test.csv")
names(test11) <- paste(names(test11),"del",sep = "_")
names(test33) <- paste(names(test33),"del",sep = "_")
names(test44) <- paste(names(test44),"del",sep = "_")
names(test11)[1] <- "file_id"
names(test33)[1] <- "file_id"
names(test44)[1] <- "file_id"
test <- test1[test3,on="file_id"][test4,on="file_id"][test2,on="file_id"][test11,on="file_id"][test33,on="file_id"][test44,on="file_id"]
test <- as.h2o(test)
pred <- h2o.predict(aml,test)

test <- as.data.frame(test)
pred <- as.data.frame(pred)
# pred$predict <- NULL
result <- data.frame(file_id=test$file_id,prob0=round(pred$p0,7),prob1=round(pred$p1,7),
                     prob2=round(pred$p2,7),prob3=round(pred$p3,7),prob4=round(pred$p4,7),
                     prob5=round(pred$p5,7))
write.csv(result,"pred_20180822_0600.csv",row.names = F,quote = F)


