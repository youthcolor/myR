library(data.table)
#library(sampling)
library(caret)
library(h2o)
h2o.init(max_mem_size = "40g")

setwd('/media/feng/文档/3rd-security/16')
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
train$label <- as.factor(train$label)

response <- "label"
predictors <- names(train)[!(names(train) %in% "label")]

index <- createDataPartition(y=train$label,p=0.7,list = F)
trainSplit <- as.h2o(train[index])
validSplit <- as.h2o(train[-index])

# Construct a large Cartesian hyper-parameter space
ntrees_opts = c(10000)       # early stopping will stop earlier
max_depth_opts = seq(6,20,2)
# min_rows_opts = c(1,5,10,20,50,100)
learn_rate_opts = seq(0.01,0.1,0.01)
sample_rate_opts = seq(0.3,1,0.05)
col_sample_rate_opts = seq(0.3,1,0.05)
col_sample_rate_per_tree_opts = seq(0.3,1,0.05)
#nbins_cats_opts = seq(100,10000,100) # no categorical features
# in this dataset

hyper_params = list( ntrees = ntrees_opts, 
                     max_depth = max_depth_opts, 
                     # min_rows = min_rows_opts, 
                     learn_rate = learn_rate_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opts
                     #,nbins_cats = nbins_cats_opts
)


# Search a random subset of these hyper-parmameters. Max runtime 
# and max models are enforced, and the search will stop after we 
# don't improve much over the best 5 random models.
search_criteria = list(strategy = "RandomDiscrete", 
                       max_runtime_secs = 21000, 
                       max_models = 100, 
                       stopping_metric = "logloss", 
                       stopping_tolerance = 0.0001, 
                       stopping_rounds = 2, 
                       seed = 123456
                       )

gbm_grid <- h2o.grid("xgboost", 
                     grid_id = "mygrid",
                     x = predictors, 
                     y = response, 
                     
                     # faster to use a 80/20 split
                     training_frame = trainSplit,
                     validation_frame = validSplit,
                     nfolds = 0,
                     
                     # alternatively, use N-fold cross-validation:
                     # training_frame = as.h2o(train),
                     # nfolds = 5,
                     
                     
                     # Gaussian is best for MSE loss, but can try 
                     # other distributions ("laplace", "quantile"):
                     # distribution="gaussian",
                     
                     # stop as soon as mse doesn't improve by 
                     # more than 0.1% on the validation set, 
                     # for 2 consecutive scoring events:
                     stopping_rounds = 2,
                     stopping_tolerance = 1e-4,
                     stopping_metric = "logloss",
                     
                     # how often to score (affects early stopping):
                     score_tree_interval = 100,
                     
                     ## seed to control the sampling of the 
                     ## Cartesian hyper-parameter space:
                     seed = 123456,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria
                     )

gbm_sorted_grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "logloss")
print(gbm_sorted_grid)

best_model <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
summary(best_model)
head(best_model@model$variable_importances,50)

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
pred <- h2o.predict(best_model,test)

test <- as.data.frame(test)
pred <- as.data.frame(pred)
# pred$predict <- NULL
result <- data.frame(file_id=test$file_id,prob0=round(pred$p0,7),prob1=round(pred$p1,7),
                     prob2=round(pred$p2,7),prob3=round(pred$p3,7),prob4=round(pred$p4,7),
                     prob5=round(pred$p5,7))
write.csv(result,"pred_20180827_0600.csv",row.names = F,quote = F)
table(pred$predict)
