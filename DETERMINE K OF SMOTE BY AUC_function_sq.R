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
AUCSMOTE1 <- function(data,class.column,N){
  library("FNN")
  minor <- data[data[,class.column]==1,-class.column]
  major <- data[data[,class.column]==0,-class.column]
  knn <- get.knn(minor,nrow(minor)-1,algorithm = "kd_tree")
  
  
  #Coverage---------------------------------
  SMOTEK <- vector('numeric',0)
  for(m in 1:nrow(minor)){
    print(paste(as.character(m),"of",as.character(nrow(minor)),"done"))
    # st <- proc.time()
    near <- knn$nn.index[m,]
    pre <- apply(minor[near,],1,function(x){(data[,1] >= min(minor[m,1],x[1]))&(data[,1] <= max(minor[m,1],x[1]))})
    
    for(i in 2:(ncol(data)-1)){
      now <- apply(minor[near,],1,function(x){(data[,i] >= min(minor[m,i],x[i]))&(data[,i] <= max(minor[m,i],x[i]))})
      pre <-pre&now
    }
    
    
    print("result 누적 시작")
    
    zero.neighbors <- rep(x = FALSE,nrow(data))
    zero.neighbors[m] <- TRUE
    
    result <- cbind(zero.neighbors,pre)
    
    for(i in 1:length(near)){
      result[,i+1] <- result[,i]|result[,i+1]
    }
    #Coverage누적
    
    print("AUC계산 시작")
    #match --------------------------------
    AUC <- vector('numeric',0)
    
    for(i in 1:nrow(minor)){
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
  
  # plot(data[,-class.column],col=data[,class.column]+1)
  # points(newset[,-class.column],col=4,pch=2)
  
  colnames(newset) <- colnames(data)
  print(SMOTEK)
  print(median(SMOTEK))
  return(rbind(data,newset))
}



