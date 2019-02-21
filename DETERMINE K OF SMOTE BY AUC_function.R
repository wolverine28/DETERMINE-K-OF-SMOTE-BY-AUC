# 
# 
# 
# data <- read.csv("overlap60%.csv",h=T)
# # data[,3] <- data[,3]-1
# plot(data[,-3],col=data[,3]+1)
# 
# res <- AUCSMOTE(data,3,200)
# 
# plot(res[,-4],col=res[,4]+1)

#class label minor:1, major:0
AUCSMOTE <- function(data,class.column,N){
  library("FNN")
  minor <- data[data[,class.column]==1,-class.column]
  major <- data[data[,class.column]==0,-class.column]
  knn <- get.knn(minor,round(nrow(minor)/2),algorithm = "kd_tree")
  
  
  #Coverage---------------------------------
  SMOTEK <- vector('numeric',0)
  for(m in 1:nrow(minor)){
    # st <- proc.time()
    near <- knn$nn.index[m,]
    cen <- matrix(unlist(apply(minor[near,],1,function(x){(minor[m,]+x)/2})),ncol = ncol(data)-1,byrow = T)
    sq.rad <- rowSums((as.matrix(minor[near,])-cen)^2)
    #중심과 반지름 계산.
    
    result <- matrix(NA,nrow = 0,ncol = nrow(data))
    # cbind(cen,sq.rad)
    for(i in 1:length(near)){
      result <- rbind(result,apply(data[,-class.column],1,function(x){sum((x-cen[i,])^2) <= sq.rad[i]}))
      print(i)
    }
    #Coverage 안데 들어오는 data 계산.
    
    
    print("result 누적 시작")
    
    zero.neighbors <- rep(x = FALSE,nrow(data))
    zero.neighbors[m] <- TRUE
    
    result <- rbind(zero.neighbors,result)
    
    for(i in 1:length(near)){
      result[i+1,] <- result[i,]|result[i+1,]
    }
    #Coverage누적
    
    result <- t(result)
    
    print("AUC계산 시작")
    #match --------------------------------
    AUC <- vector('numeric',0)
    
    for(i in 1:round(nrow(minor)/2)){
      tab <- table(data[,class.column],result[,i])
      TPR <- tab[1,1]/sum(tab[,1])
      FPR <- tab[1,2]/sum(tab[,2])
      AUC <- c(AUC,0.5+TPR/2-FPR/2)
    }
    
    plot(AUC,type='l')
    title(main=as.character(m))
    SMOTEK <- c(SMOTEK,which.max(AUC)-1)
    # print(proc.time()-st)
  }
  
  
  # # SMOTE
  
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
  
  plot(data[,-class.column],col=data[,class.column]+1)
  points(newset[,-class.column],col=4,pch=2)
  
  colnames(newset) <- colnames(data)
  return(rbind(data,newset))
}



