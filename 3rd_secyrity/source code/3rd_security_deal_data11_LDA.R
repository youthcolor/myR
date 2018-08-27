library(data.table)
#library(sampling)
library(caret)

setwd('/home/feng/Desktop/3rd_security')
train1 <- fread("file_id_apiapi_49_select_N_train.csv")
train2 <- fread("file_id_label_select_train.csv")
train3 <- fread("file_id_rv_61_select_N_train.csv")
train4 <- fread("file_id_api_seq_66_select_N_train_0.csv")

# write.csv(train,"train_115.csv",row.names = F,quote = F)

train <- train1[train3,on="file_id"][train4,on="file_id"][train2,on="file_id"]
#train <- train1[train2,on="file_id"]
train[,file_id:=NULL]
train$label <- as.factor(train$label)









names(train)
# Scale dataset
print( "... scaling dataset ..." )
maxs = apply( train[,-187], 2, max )
mins = apply( train[,-187], 2, min )
dataset = as.data.frame( scale( train[,-187], center = mins, scale = maxs - mins ) )
dataset = cbind( dataset, "label" = train$label )
index <- createDataPartition(y=dataset$label, p=0.6, list=F)
X_train = dataset[ index, ]
test = dataset[ -index, ]

# Dimensionality Reduction -> LDA   
print( "... dimensionality reduction - LDA ..." )
library( MASS )
lda = lda( label ~ ., data = X_train )
# Ploting LDA Model
projected_data = as.matrix( X_train[, 1:190] ) %*% lda$scaling
plot( projected_data, col = X_train[,191], pch = 24 )
# Testing
X_test = test[, !( names( test ) %in% c( "label" ) ) ]  
model.results = predict( lda, X_test )
# Results
print( "... resulting ..." )

# Confusion Matrix
library( caret )
t = table( model.results$class, test$label )
print( confusionMatrix( t ) )
