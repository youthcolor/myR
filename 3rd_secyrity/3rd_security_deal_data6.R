library(data.table)

# library(h2o)
# h2o.init(max_mem_size = "15g")

setwd('/home/feng/Desktop/3rd_security')
train <- fread("id_label_api_seq_Ns_select0.6.csv")
head(train)
n <- sum(train$api_seq_N)
n0 <- sum(train[label==0,]$api_seq_N)
n1 <- sum(train[label==1,]$api_seq_N)
n2 <- sum(train[label==2,]$api_seq_N)
n3 <- sum(train[label==3,]$api_seq_N)
n4 <- sum(train[label==4,]$api_seq_N)
n5 <- sum(train[label==5,]$api_seq_N)
nc <- c(n0,n1,n2,n3,n4,n5)
rm(n0,n1,n2,n3,n4,n5)

train[,c("file_id","label_N"):=NULL]


# 求熵
getEntropy <- function(x){
  v = 0
  for(i in 0:5){
    if(nrow(train[api_seq==x&label==i])>0){
      p_vt_c = train[api_seq==x&label==i]$api_seq_N / nc[i+1]
      p_vt = sum(train[api_seq==x]$api_seq_N) / n
      p_c = nc[i+1] / n
      v = v + p_vt_c*log10(p_vt_c / (p_vt*p_c))
    }else{
      p_vt_c = 1
      p_vt = 1 - (sum(train[api_seq==x]$api_seq_N) / n)
      p_c = nc[i+1] / n
      v = v + p_vt_c*log10(p_vt_c / (p_vt*p_c))
    }
  }
  return(v)
}

api_seqs <- unique(train$api_seq)
api_seq_entropys <- c()
for(i in 1:length(api_seqs)){
  api_seq_entropys[i] <- getEntropy(api_seqs[i])
  print(i)
}

data <- data.frame(api_seq = api_seqs,api_seq_entropy = api_seq_entropys)
head(data)
write.csv(data,"api_seq_entropy_select0.6.csv",row.names = F,quote = F)
summary(data)

api_seqs <- unique(train$api_seq)
df_api_seq <- data.table(apiseq=api_seqs)
df_api_seq[,api_seq_entropy:=getEntropy(apiseq)]







# 求熵
getEntropy2 <- function(x){
  v = 0
  for(i in 0:5){
    if(nrow(train[api_seq==x&label==i])>0){
      p_vt_c = train[api_seq==x&label==i]$api_seq_N / nc[i+1]
      p_vt = sum(train[api_seq==x]$api_seq_N) / n
      p_c = nc[i+1] / n
      v = v + p_vt_c*log10(p_vt_c / (p_vt*p_c))
    }else{
      p_vt_c = 1
      p_vt = 1 - (sum(train[api_seq==x]$api_seq_N) / n)
      p_c = nc[i+1] / n
      v = v + p_vt_c*log10(p_vt_c / (p_vt*p_c))
    }
  }
  print(x)
  return(c(x,v))
}
api_seqs <- unique(train$api_seq)


#----
library(parallel)
# 用system.time来返回计算所需时间
system.time({
  x <- api_seqs
  cl <- makeCluster(4) # 初始化四核心集群
  results <- parLapply(cl,x,train,getEntropy2) # lapply的并行版本
  res.df <- do.call('rbind',results) # 整合结果
  stopCluster(cl) # 关闭集群
})



#-----用一个实力来演示 R 如何多线程计算
func <- function(x) {
  n = 1
  raw <- x
  while (x > 1) {
    x <- ifelse(x%%2==0,x/2,3*x+1)
    n = n + 1
  }
  return(c(raw,n))
}

#----
library(parallel)
# 用system.time来返回计算所需时间
system.time({
  x <- 1:1e5
  cl <- makeCluster(4) # 初始化四核心集群
  results <- parLapply(cl,x,func) # lapply的并行版本
  res.df <- do.call('rbind',results) # 整合结果
  stopCluster(cl) # 关闭集群
})
