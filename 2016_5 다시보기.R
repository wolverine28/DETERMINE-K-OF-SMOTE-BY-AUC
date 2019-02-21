IN <- function(x,y,n){
  o <- (x+y)/2
  r <- sqrt(sum((x-y)^2))/2
  IN <- sqrt(rowSums((as.matrix(data[,-3])-matrix(o,nrow=n,ncol=length(x),byrow = T))^2)) < r
  return(IN)
}
setwd("Z:/공동작업/Imbalance classification/Experiment (1)/data")
setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/synthetic")
data <- read.csv("03subcl5-800-7-50-BI.dat.csv")
data[,3] <- !data[,3]
plot(data[,-3],col=data[,3]+1)


data_MN <- as.matrix(data[data[,3]==1,])[,-3]



N <- nrow(data)
min_n <- nrow(data_MN)
P <- matrix(NA,min_n,min_n)
for(i in 1:min_n){
  for(j in i:min_n){
    x <- data_MN[i,]
    y <- data_MN[j,]
    
    res <- IN(x,y,N)
    real <- data[,3]
    
    TP <- sum((res==1)&(real==1))
    FP <- sum((res==1)&(real==0))
    TN <- sum((res==0)&(real==0))
    FN <- sum((res==0)&(real==1))
    Precis <- TP/(TP+FP)
    
    P[i,j] <- Precis
  }
}

idx <- cbind(which(upper.tri(P),arr.ind = T),P[upper.tri(P)])
idx <- idx[order(idx[,3],decreasing = T),]
idx <- idx[!is.nan(idx[,3]),]
plot(idx[,3])

for(j in seq(0.1,0.9,length.out = 10)){
  can <- idx[idx[,3]>j,]
  can <- can[!is.na(can[,3]),1:2]
  plot(data[,-3],col=data[,3]+1)
  for(i in 1:nrow(can)){
    l <- data_MN[can[i,],]
    segments(x0 = l[1,1],y0 = l[1,2],x1 = l[2,1],y1 = l[2,2],col = 2)
    title(main = j)
  }
}
