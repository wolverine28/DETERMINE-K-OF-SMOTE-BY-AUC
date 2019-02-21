library("FNN")

dist.2p <- function(x,y){
  return(sqrt(sum((x-y)^2)))
}

data <- read.csv("dataset2.csv",h=T)
plot(data[,-3],col=data[,3]+1)


minor <- data[data[,3]==1,-3]
major <- data[data[,3]==0,-3]
knn <- get.knn(minor,nrow(minor)-1,algorithm = "kd_tree")


#Coverage---------------------------------
SMOTEK <- vector('numeric',0)
for(m in 1:nrow(minor)){
  near <- knn$nn.index[m,]
  cen <- matrix(unlist(apply(minor[near,],1,function(x){(minor[m,]+x)/2})),ncol = 2,byrow = T)
  sq.rad <- rowSums((as.matrix(minor[near,])-cen)^2)
  
  
  result <- matrix(NA,nrow = 0,ncol = nrow(data))
  for(i in 1:length(near)){
    result <- rbind(result,apply(data[,-3],1,function(x){sum((x-cen[i,])^2) <= sq.rad[i]}))
  }
  
  zero.neighbors <- rep(x = FALSE,nrow(data))
  zero.neighbors[m] <- TRUE
  
  result <- rbind(zero.neighbors,result)
  
  for(i in 1:length(near)){
    result[i+1,] <- result[i,]|result[i+1,]
  }
  
  result <- t(result)
  #match --------------------------------
  AUC <- vector('numeric',0)
  
  for(i in 1:nrow(minor)){
    tab <- table(data[,3],result[,i])
    TPR <- tab[1,1]/sum(tab[,1])
    FPR <- tab[1,2]/sum(tab[,2])
    AUC <- c(AUC,0.5+TPR/2-FPR/2)
  }
  
  plot(AUC,type='l')
  title(main=as.character(m))
  SMOTEK <- c(SMOTEK,which.max(AUC)-1)
}


# # SMOTE
# SMOTEK
# [1] 9 1 9 2 1 2 1 1 1 1


N <- 200
newset <- matrix(NA,nrow = 0,ncol = ncol(data))
for(i in 1:(N/100)){
  for(j in 1:nrow(minor)){
    if(SMOTEK[j]==0){
      tmp <- unlist(c(minor[j,],1))
      newset <- rbind(newset,tmp)
    }
    else{
      candidate <- knn$nn.index[j,1:SMOTEK[j]]
      if(length(candidate)==1){
        choose <- candidate
      }
      else{
        choose <- sample(candidate,1)
      }
      new <- minor[j,]+runif(1)*(minor[choose,]-minor[j,])
      newset <- rbind(newset,unlist(c(new,1)))
    }
  }
}

plot(data[,-3],col=data[,3]+1)
points(newset[,-3],col=4,pch=2)

finaldata <- rbind(data,newset)
plot(finaldata[,-3],col=finaldata[,3]+1)
