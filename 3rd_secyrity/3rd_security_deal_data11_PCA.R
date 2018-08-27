library(data.table)
#library(sampling)
library(caret)
library(h2o)
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
train$label <- as.factor(train$label)
train_labels <- train$label
train_scale <- scale(train[,-c("label")])
train_new <- data.frame(label=train_labels,train_scale)
index <- createDataPartition(y=train_new$label,p=0.6,list = F)
train_hex <- train_new[index,]
test_hex <- train_new[-index,]
record_model <- h2o.deeplearning(x = 2:591, y = 1,
                                 training_frame=as.h2o(train_hex), 
                                 validation_frame = as.h2o(test_hex),
                                 activation = "RectifierWithDropout", 
                                 hidden = c(1024,1024,1024),
                                 epochs = 100, 
                                 l1 = 1e-5, 
                                 input_dropout_ratio = 0.2,
                                 rate = 0.1,
                                 verbose = T
                                 )

## Dimensionality Reduction 
# PCA
print( "... principal component analysis ..." )
pca_label <- train$label
pca_trainset = train[,label:=NULL]

pca = prcomp( pca_trainset, scale = T )
pr_var = (pca$sdev)^2 # variance
prop_varex = pr_var / sum( pr_var )
plot( prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b" ) #scree plot
plot( cumsum( prop_varex ), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = "b" )
# Creating a new dataset
train = data.frame( label = pca_label, pca$x )

new_trainset = train[, 1:101]
new_trainset$label <- as.factor(new_trainset$label)



# aml <- h2o.automl(y = "label", training_frame = as.h2o(new_trainset), 
#                   max_runtime_secs = 1000)



## Build the neural network (NN)
library( neuralnet )
print( "... training ..." )
n = names( new_trainset )
f = as.formula( paste( "label ~", paste( n[!n %in% "label" ], collapse = "+" ) ) )
nn = neuralnet( f, new_trainset, hidden =6, linear.output = FALSE, threshold=0.01 )

## Plot the NN
#plot( nn, rep = "best" )

## Test the resulting output
print( "... testing ..." )
nn.results = compute( nn, new_testset)

## Results
results = data.frame( actual = tests_label, 
                      prediction = round( nn.results$net.result ) )

mse = sum(( results$actual - results$prediction )^2 ) / nrow( results )
print( paste( "mean square error: ",  mse ) )

## Confusion Matrix
library( caret )
t = table( results ) 
print( confusionMatrix( t ) )
h2o.removeAll()
m2 <- h2o.deeplearning(
  model_id="dl_model_faster", 
  training_frame=as.h2o(new_trainset), 
  #validation_frame=valid,
  #x=predictors,
  y="label",
  hidden=c(32,32,32,32),                  ## small network, runs faster
  epochs=10000,                      ## hopefully converges earlier...
  #score_validation_samples=10000,      ## sample the validation dataset (faster)
  #stopping_rounds=2,
  stopping_metric="logloss", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.001
)
summary(m2)
plot(m2)


dlmodel <- h2o.deeplearning(
  #x=predictors,
  y="label",
  training_frame=as.h2o(new_trainset), 
  hidden=c(512,128,64,32),
  epochs=1,
  nfolds=5,
  fold_assignment="Modulo" # can be "AUTO", "Modulo", "Random" or "Stratified"
)
dlmodel

hyper_params <- list(
  hidden=list(c(32,32,32,32),c(64,64,64)),
  input_dropout_ratio=c(0,0.05,0.1),
  rate=c(0.001,0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)
hyper_params
grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid", 
  training_frame=as.h2o(new_trainset), 
  #validation_frame=valid, 
  #x=predictors, 
  y="label",
  epochs=10,
  stopping_metric="misclassification",
  stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7, 
  l1=1e-5,
  l2=1e-5,
  activation=c("Rectifier"),
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params=hyper_params
)
grid
