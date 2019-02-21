
AUCSMOTE <- function(data,class.column,N){
  library("FNN")
  minor <- data[data[,class.column]==1,-class.column]
  major <- data[data[,class.column]==0,-class.column]
  knn <- get.knn(minor,nrow(minor)-1,algorithm = "kd_tree")
  
  SMOTEK <- vector('numeric',0)
  pre <- list()
  print("result 누적 시작")
  for(m in 1:(nrow(minor)-1)){
    print(paste(as.character(m),"of",as.character(nrow(minor)),"done"))
    # st <- proc.time()
    
    near <- (m+1):nrow(minor)
    # print(near)
    pre[[m]] <- apply(minor[near,],1,function(x){(data[,1] >= min(minor[m,1],x[1]))&(data[,1] <= max(minor[m,1],x[1]))})
    colnames(pre[[m]]) <- near
    for(i in 2:(ncol(data)-1)){
      now <- apply(minor[near,],1,function(x){(data[,i] >= min(minor[m,i],x[i]))&(data[,i] <= max(minor[m,i],x[i]))})
      pre[[m]] <-pre[[m]]&now
    }
  }
  
  
  for(m in 1:nrow(minor)){
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
    
    
    print("AUC계산 시작")
    #match --------------------------------
    # AUC <- vector('numeric',0)
    # table(result[,5],data[,class.column])[c(2,1),c(2,1)]
    
    table <- t(apply(result,2,function(x){as.vector(table(x,data[,class.column])[c(2,1),c(2,1)])}))
    colnames(table) <-c("TP","FN","FP","TN")
    
    Precis <- table[,1]/(table[,1]+table[,3])
    Recall <- TPR <- table[,1]/(table[,1]+table[,2])
    FPR <- table[,3]/(table[,3]+table[,4])
    
    
    # AUC.ROC <- 0.5+TPR/2-FPR/2
    # AUC.PR <- (Precis+1)*Recall/2+Precis*(1-Recall)/2
    
    d <- Precis[2:length(Precis)]-Precis[1:(length(Precis)-1)]
    
    
    if(sum(d)==0)
      md <- nrow(minor)-1
    else{
      
      md <- which(d==min(d[which(d<0)]))
    }
    
    
    plot(TPR,Precis,xlim=c(0,1),ylim=c(0,1),type='l')
    # plot(FPR,TPR,xlim=c(0,1),ylim=c(0,1),type='l')
    # points(FPR[md],TPR[md],pch=16,col=2)
    #     plot(0.5+TPR/2-FPR/2,type='l',ylim=c(0,1))
    #     lines((Precis+1)*Recall/2+Precis*(1-Recall)/2,type='l',ylim=c(0,1),col=2)
    title(main=as.character(m))
    #     legend(nrow(minor)-20,1,c("PR","ROC"), lty=c(1,1), lwd=c(2,2),col=c("red","black"))
    #     abline(v=which.max(AUC.PR),col="red")
    #     abline(v=which.max(AUC.ROC),col="black")
    #     plot(table[,1],ylim=c(0,1000),type="l",col=1)
    #     lines(table[,2],col=2)
    #     lines(table[,3],col=3)
    #     lines(table[,4],col=4)
    
    
    # SMOTEK <- c(SMOTEK,which.max(AUC.ROC)-1)
    SMOTEK <- c(SMOTEK,(md-1))
    
  }
  
  # SMOTEK[is.na(SMOTEK)]<-6
  print(SMOTEK)
  # print(proc.time()-st)
  # # SMOTE
  
  newset <- matrix(NA,nrow = 0,ncol = ncol(data))
  for(i in 1:(N/100)){
    for(j in 1:nrow(minor)){
      if(SMOTEK[j]==0){
#         tmp <- unlist(c(minor[j,],1))
#         newset <- rbind(newset,tmp)
        next
      }
      else{
        candidate <- knn$nn.index[j,1:SMOTEK[j]]
        if(length(candidate)==1){
          choose <- candidate
        }
        else{
          choose <- sample(candidate,1)
        }
        new <- minor[j,]+runif(ncol(minor))*(minor[choose,]-minor[j,])
        newset <- rbind(newset,unlist(c(new,1)))
      }
    }
  }
  
  # plot(data[,-class.column],col=data[,class.column]+1)
  # points(newset[,-class.column],col=4,pch=2)
  
  colnames(newset) <- colnames(data)
  
  print(median(SMOTEK))
  return(newset)
}



