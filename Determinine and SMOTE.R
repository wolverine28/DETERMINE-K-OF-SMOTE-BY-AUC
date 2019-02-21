
determineK <- function(data,class.column){
  library("FNN")
  library("BBmisc")
  minor <- data[data[,class.column]==1,-class.column]
  major <- data[data[,class.column]==0,-class.column]
  knn <- get.knn(minor,nrow(minor)-1,algorithm = "kd_tree")
  
  
  pre <- list()
  print("result 누적 시작")
  for(m in 1:(nrow(minor)-1)){
    if(m%%10==0)
      print(paste(as.character(m),"of",as.character(nrow(minor)),"done"))

    near <- (m+1):nrow(minor)
    pre[[m]] <- apply(minor[near,],1,function(x){data[,1] %btwn% c(x[1],minor[m,1])})
    colnames(pre[[m]]) <- near
    for(i in 2:(ncol(data)-1)){
      now <- apply(minor[near,],1,function(x){data[,i] %btwn% c(x[i],minor[m,i])})
      pre[[m]] <-pre[[m]]&now
    }

  }
  
  SMOTEK <- vector('numeric',0)
  print("AUC계산 시작")
  for(m in 1:nrow(minor)){
    if(m%%10==0)
      print(m)
    near <- knn$nn.index[m,]
    if(m %in% near){
      near[near==m] <- (1:nrow(minor))[!(1:nrow(minor) %in% near)]
    }
    result <- matrix(NA,nrow = nrow(data),ncol = nrow(minor))
    zero.neighbors <- rep(x = FALSE,nrow(data))
    zero.neighbors[which(data[,class.column]==1)[m]] <- TRUE
    result[,1] <- zero.neighbors
    idx <- m
    for(i in 1:(nrow(minor)-1)){
      n <- near[i]
      if(m>n){
        t<-m
        m<-n
        n<-t
      }
      result[,i+1] <- pre[[m]][,n-m]
      m <- idx
    }
    
    for(i in 1:length(near)){
      result[,i+1] <- result[,i]|result[,i+1]
    }
    #Coverage누적
    
    
    
    #match --------------------------------
    table <- t(apply(result,2,function(x){as.vector(table(x,data[,class.column])[c(2,1),c(2,1)])}))
    colnames(table) <-c("TP","FN","FP","TN")
    
    Precis <- table[,1]/(table[,1]+table[,3])
    Recall <- TPR <- table[,1]/(table[,1]+table[,2])
    FPR <- table[,3]/(table[,3]+table[,4])
    
    
    # AUC.ROC <- 0.5+TPR/2-FPR/2
    # AUC.PR <- (Precis+1)*Recall/2+Precis*(1-Recall)/2
    
    d <- Precis[2:length(Precis)]-Precis[1:(length(Precis)-1)]
    
    
    if(sum(d)==0){
      md <- nrow(minor)-1
    } else{
      
      md <- which(d==min(d[which(d<0)]))[1]
    }
#     plot(TPR,Precis,xlim=c(0,1),ylim=c(0,1),type='l')
#     title(main=as.character(m))
    
    print(md)
    SMOTEK <- c(SMOTEK,(md-1))
    
  }

  return(SMOTEK)
}

doSMOTE <- function(data,class.column,N,SMOTEK){
  
  library("FNN")
  minor <- data[data[,class.column]==1,-class.column]
  major <- data[data[,class.column]==0,-class.column]
  knn <- get.knn(minor,nrow(minor)-1,algorithm = "kd_tree")
  
  
  resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 
  
  make.idx <- which(SMOTEK!=0)
  make <- cbind(make.idx,SMOTEK[make.idx])
  candi <- apply(make,1,function(x){resamp(knn$nn.index[x[1],1:x[2]],1)})
  
  for(i in 1:nrow(make))
    print(resamp(knn$nn.index[make[i,1],1:make[i,2]],1))
  
  newset <- matrix(NA,nrow = 0,ncol = ncol(data))
  for(i in 1:(N/100)){
    new <- cbind(minor[make.idx,]+runif(ncol(minor))*(minor[candi,]-minor[make.idx,]),1)
    colnames(newset) <- colnames(new)
    newset <- rbind(newset,new)
  }
  
  colnames(newset) <- colnames(data)
  
  return(newset)
}

ONENN <- function(res,testset){
  fit <- knn(res[,-col],testset[,-col],res[,col],k=1)
  tab <- table(fit,testset[,col])[2:1,2:1]
  TPR <-Recall <- tab[1,1]/sum(tab[,1])
  FPR <- tab[1,2]/sum(tab[,2])
  Precis <- tab[1,1]/sum(tab[1,])
  
  AUCROC <- 0.5+TPR/2-FPR/2
  AUCPR <- (Precis+1)*Recall/2+Precis*(1-Recall)/2
  Fm <- 2*Precis*Recall/(Precis+Recall)
  Gm <- sqrt(Precis*Recall)
  
  output <- c(AUCROC,AUCPR,Fm,Gm)
  
  return(output)
}

ROS <- function(data,target.column,N){
  minNum <- sum(data[,target.column]==1)
  minor <- data[data[,target.column]==1,]
  return(minor[sample(minNum,replace = T,minNum*N/100),])
}

prplot <- function(data){
  library("rgl")
  pr <- princomp(data[,-col])
  plot(pr$sdev)
  prs <- pr$scores
  plot3d(prs[,1:3],col=data[,col]+1)
}