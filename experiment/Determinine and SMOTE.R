
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
    
#     postscript(paste("minor ",as.character(m),"- Pre",".eps",sep=""))
#     par(mar=c(5.1,4.1,4.1,2.1))
#     par(mar=c(3.5,3.5,2,1.5))
    
#     # plot(c(TPR),c(Precis),xlim=c(0,1),ylim=c(0,1),type='l',ylab = "Precision",xlab="Recall",lwd=1.5)
#     plot(c(Precis),type='l',xlab="",ylab = "",lwd=1.5)
#     mtext(side = 1, text = "Neighborhood Size", line = 2.2)
#     mtext(side = 2, text = "Precision", line = 2.2)
#     if(m==1){
#       title(paste(main=as.character(m),"st Minority",sep=""),line = 0.5)
#     }else {
#       title(paste(main=as.character(m),"th Minority",sep=""),line = 0.5)
#     }
#     dev.off()
#     # print(md)
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
  
  
#   for(i in 1:nrow(make))
#     print(resamp(knn$nn.index[make[i,1],1:make[i,2]],1))
#   
  newset <- matrix(NA,nrow = 0,ncol = ncol(data))
  for(i in 1:(N/100)){
    candi <- apply(make,1,function(x){resamp(knn$nn.index[x[1],1:x[2]],1)})
    new <- cbind(minor[make.idx,]+runif(ncol(minor))*(minor[candi,]-minor[make.idx,]),1)
    colnames(newset) <- colnames(new)
    newset <- rbind(newset,new)
  }
  
  colnames(newset) <- colnames(data)
  
  return(newset)
}
doADA <- function(data,class.column,SMOTEK){
  resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 
  library("FNN")
  ms <- sum(data[,class.column]==1)
  ml <- sum(data[,class.column]==0)
  minor <- data[data[,class.column]==1,-class.column]
  maj.idx <- which(data[,class.column]==0)
  
  
  make.idx <- which(SMOTEK!=0)
  knn <- get.knnx(data[,-class.column],minor,k = nrow(minor)+1 ,algorithm = "kd_tree")$nn.index[,-1]
  tmp <- cbind(SMOTEK[make.idx],knn[make.idx,])
  
  neigh <- apply(tmp,1,function(x){x[2:(x[1]+1)]})
  
  d <- ms/ml
  G <- (ml-ms)
  
  r <- sapply(neigh,function(x){sum(x%in%maj.idx)})/SMOTEK[make.idx]
  if(!sum(r)){
    new <- matrix(NA,ncol=ncol(minor)+1,nrow=0)
    return(new)
  }
  
  r <-r/sum(r)
  
  g <- round(r*G)
  if(!sum(g)){
    new <- matrix(NA,ncol=ncol(minor)+1,nrow=0)
    return(new)
  }
  makeg <- which(g!=0)
  
  mg <- g[makeg]
  m.idx <- make.idx[makeg]
  mminor <- minor[makeg,]
  
  
  knn_m <- get.knn(minor,k = nrow(minor)-1,algorithm = "kd_tree")$nn.index
  tmp <- cbind(m.idx,SMOTEK[m.idx],mg)
  tmp <- matrix(unlist(apply(tmp,1,function(x){rep(x[1:2],x[3])})),ncol=2,byrow=T)
  
  
  candi <- apply(tmp,1,function(x){resamp(knn_m[x[1],1:x[2]],1)})
  # print(candi)
  newset <- cbind(minor[tmp[,1],]+(minor[candi,]-minor[tmp[,1],])*runif(ncol(minor)),1)
  colnames(newset) <- colnames(data)
  return(newset) 
}
doBSMOTE1 <- function(data, class.column, SMOTEK, s) {
  
  
  library(FNN)
  # class.column : column of the input data containing class labels
  # m : number of nearest neighbors for obtaining DANGER objects
  # k : number of nearest neighbors for reference objects
  # s : number of newly generated objects for each object in DANGER
  
  y <- data[ ,class.column]
  
  tmp <- table(y)
  n <- sum(tmp)
  
  
  y.mn <- which.min(tmp)
  n.mn <- tmp[y.mn]
  n.mj <- n - n.mn
  y.mn <- as.numeric(names(y.mn))
  ind.mn <- which(y == y.mn)
  ind.mj <- 1:n
  ind.mj <- ind.mj[-ind.mn]
  
  data <- data[,-class.column]
  data <- as.matrix(data)
  
  ##################################### 
  
  minor <- data[ind.mn,]
  m <- round(nrow(minor))
  nn <- get.knnx(data = data,
                 query = data[ind.mn,],
                 k = m+1, ## 수정 JS 자기자신도 선택되고 있었음
                 algorithm = "kd_tree")$nn.index[,-1]
  r <- apply(X = nn,
             MARGIN = 1,
             FUN = function(x) sum(x %in% ind.mj))
  r <- r/m
  
  
  ind.safe <- which(r < 0.5)
  ind.noisy <- which(r == 1)
  ind.danger <- ind.mn[-c(ind.safe, ind.noisy)]
  ind.safe <- ind.mn[ind.safe]
  ind.noisy <- ind.mn[ind.noisy]
  
  if(length(ind.danger)==0){
    synTotal <- matrix(NA,ncol = ncol(data)+1,nrow = 0)
    
    return(synTotal)
  }
  
  
  temp <- length(ind.danger)
  print(paste(temp, " (", (temp/n.mn)*100, "%) ",
              "samples are in DANGER", sep=""))
  temp <- temp * s
  print(paste(temp,"synthetic samples are generated"))
  
  
  dangerNN <- get.knnx(data = data[ind.mn,],
                       query = matrix(data[ind.danger,],ncol=ncol(data),byrow=T),
                       k = n.mn-1, ## 수정 JS 자기자신도 선택되고 있었음
                       algorithm = "kd_tree")$nn.index[,-1]
  
  dangerNN <- matrix(dangerNN,ncol=n.mn-2,byrow=T)
  make <- cbind(1:length(ind.danger),SMOTEK[ind.danger])
  
  
  
  
  resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 
  newset <- matrix(NA,nrow = 0,ncol = ncol(data)+1)
  for(i in 1:s){
    candi <- apply(make,1,function(x){resamp(dangerNN[x[1],1:x[2]],1)})
    ttmmpp <- minor[ind.danger,]+runif(ncol(minor))*(minor[candi,]-minor[ind.danger,])
    ttmmpp <- matrix(ttmmpp,ncol=ncol(data),byrow=F)
    new <- cbind(ttmmpp,1)
    colnames(new) <- colnames(newset)
    newset <- rbind(newset,new)
  }
  
  
  return(newset)
  
}

ONENN <- function(res,testset){
  fit <- knn(res[,-col],testset[,-col],res[,col],k=1)
  ##
  tmp <- cbind(testset[,col], fit)
  tp1 <- length(which(tmp[,1] == 1 & tmp[,2] == 1))
  fp1 <- length(which(tmp[,1] == 0 & tmp[,2] == 1))
  tn1 <- length(which(tmp[,1] == 0 & tmp[,2] == 0))
  fn1 <- length(which(tmp[,1] == 1 & tmp[,2] == 0))
  TPR<- Recall <- tp1 / (tp1 + fn1)
  FPR <- fp1 / (tn1 + fp1)
  Precis <- tp1 / (tp1 + fp1)
  TNR <- tn1 / (tn1 + fp1)
  
  ##
  #   tab <- table(pred,testset[,col])[2:1,2:1]
  #   TPR <-Recall <- tab[1,1]/sum(tab[,1])
  #   FPR <- tab[1,2]/sum(tab[,2])
  #   Precis <- tab[1,1]/sum(tab[1,])
  
  AUCROC <- 0.5+TPR/2-FPR/2
  AUCPR <- (Precis+1)*Recall/2+Precis*(1-Recall)/2
  Fm <- 2*Precis*Recall/(Precis+Recall)
  Gm <- sqrt(TNR*Recall)
  
  output <- c(AUCROC,AUCPR,Fm,Gm)
  
  return(output)
}
CART <- function(res,testset){
  library("rpart")
  res <- as.data.frame(res)
  res[,col] <- as.factor(res[,col])
  fit <- rpart(formula(paste(colnames(res)[col],"~.")),res)
  pred <- predict(fit,testset[,-col],type="class")
  
  pred <- as.numeric(as.character(pred))
  
  
  ##
  tmp <- cbind(testset[,col], pred)
  tp1 <- length(which(tmp[,1] == 1 & tmp[,2] == 1))
  fp1 <- length(which(tmp[,1] == 0 & tmp[,2] == 1))
  tn1 <- length(which(tmp[,1] == 0 & tmp[,2] == 0))
  fn1 <- length(which(tmp[,1] == 1 & tmp[,2] == 0))
  TPR<- Recall <- tp1 / (tp1 + fn1)
  FPR <- fp1 / (tn1 + fp1)
  Precis <- tp1 / (tp1 + fp1)
  TNR <- tn1 / (tn1 + fp1)
  
  ##
#   tab <- table(pred,testset[,col])[2:1,2:1]
#   TPR <-Recall <- tab[1,1]/sum(tab[,1])
#   FPR <- tab[1,2]/sum(tab[,2])
#   Precis <- tab[1,1]/sum(tab[1,])
  
  AUCROC <- 0.5+TPR/2-FPR/2
  AUCPR <- (Precis+1)*Recall/2+Precis*(1-Recall)/2
  Fm <- 2*Precis*Recall/(Precis+Recall)
  Gm <- sqrt(TNR*Recall)
  
  output <- c(AUCROC,AUCPR,Fm,Gm)
  
  return(output)
}

BSMOTE1 <- function(data, target.column, m, k, s) {
  
  
  library(FNN)
  # target.column : column of the input data containing class labels
  # m : number of nearest neighbors for obtaining DANGER objects
  # k : number of nearest neighbors for reference objects
  # s : number of newly generated objects for each object in DANGER
  
  
  if (k < s) {
    s <-k-1
  }
  
  
  y <- data[ ,target.column]
  if (is.character(y) == TRUE) {
    y <- as.factor(y)
    y <- as.numeric(y)
  } else if (is.factor(y) == TRUE) {
    y <- as.numeric(y)
  }
  tmp <- table(y)
  n <- sum(tmp)
  if (length(tmp) != 2) {
    stop("Error : not for the binary classification")
  }
  
  
  y.mn <- which.min(tmp)
  n.mn <- tmp[y.mn]
  n.mj <- n - n.mn
  y.mn <- as.numeric(names(y.mn))
  ind.mn <- which(y == y.mn)
  ind.mj <- 1:n
  ind.mj <- ind.mj[-ind.mn]
  if (length(ind.mn) < k) {
    stop("Error : k must be smaller than the number of minority examples")
  }
  
  
  data <- data[,-target.column]
  data <- as.matrix(data)
  if (is.character(data) == TRUE) {
    stop("Error : non-numeric values in feature vectors")
  }
  if (sum(is.na(data)) != 0) {
    stop("Error : NAs in feature vector")
  }
  
  
  nn <- get.knnx(data = data,
                 query = data[ind.mn,],
                 k = m+1, ## 수정 JS 자기자신도 선택되고 있었음
                 algorithm = "kd_tree")$nn.index[,-1]
  r <- apply(X = nn,
             MARGIN = 1,
             FUN = function(x) sum(x %in% ind.mj))
  r <- r/m
  
  
  ind.safe <- which(r < 0.5)
  ind.noisy <- which(r == 1)
  ind.danger <- ind.mn[-c(ind.safe, ind.noisy)]
  ind.safe <- ind.mn[ind.safe]
  ind.noisy <- ind.mn[ind.noisy]
  if(length(ind.danger)==0){
    tmp <- list()
    tmp$d.synTotal <- matrix(NA,ncol = ncol(data),nrow = 0)
    
    return(tmp)
  }
  
  temp <- length(ind.danger)
  print(paste(temp, " (", (temp/n.mn)*100, "%) ",
              "samples are in DANGER", sep=""))
  temp <- temp * s
  print(paste(temp,"synthetic samples are generated"))
  
  
  d.synIndvd <- list()
  itr <- length(ind.danger)
  dangerNN <- get.knnx(data = data[ind.mn,],
                       query = matrix(data[ind.danger,],ncol=ncol(data),byrow = T),
                       k = k+1, ## 수정 JS 자기자신도 선택되고 있었음
                       algorithm = "kd_tree")$nn.index[,-1]
  dangerNN <- matrix(dangerNN,ncol=k,byrow = T)
  for (i in 1:itr) {
    
    x <- ind.danger[i]
    x <- data[x,]
    
    myNN <- dangerNN[i,]
    myNN <- ind.mn[myNN]
    myNN <- sample(x = myNN, size = s)
    myNN <- data[myNN,]
    
    syn <- matrix(data = NA, nrow = 0, ncol = ncol(data))
    for (j in 1:s) {
      syn <- rbind(syn, x + (runif(1) * (matrix(myNN,ncol=target.column-1,byrow=T)[j,] - x)))
    }
    d.synIndvd[[i]] <- syn
    
  }
  
  
  names(d.synIndvd) <- ind.danger
  d.synTotal <- matrix(NA,ncol=ncol(data),nrow=0)
  # d.synTotal <- matrix(data = unlist(d.synIndvd), ncol = ncol(data), byrow = F) ## 수정 JS
  for(i in 1:length(d.synIndvd)){
    d.synTotal <- rbind(d.synTotal,d.synIndvd[[i]])
  }
  status <- list()
  status[[1]] <- ind.danger
  status[[2]] <- ind.safe
  status[[3]] <- ind.noisy
  names(status) <- c("DANGER","SAFE","NOISY")
  result <- list(d.synTotal, d.synIndvd, status)
  names(result) <- c("d.synTotal", "d.synIndvd", "status")
  return(result)
  
  
}
ADASYN <- function(data,class.column,dth,beta,k){
  library("FNN")
  ms <- sum(data[,class.column]==1)
  ml <- sum(data[,class.column]==0)
  minor <- data[data[,class.column]==1,]
  maj.idx <- which(data[,class.column]==0)
  knn <- get.knnx(data[,-class.column],minor[,-class.column],k = k+1,algorithm = "kd_tree")$nn.index[,-1]
  knn_m <- get.knn(minor[,-class.column],k = k,algorithm = "kd_tree")$nn.index
  d <- ms/ml
  if(d < dth){
    G <- (ml-ms)*beta
    r <- apply(knn ,1,function(x){sum(x%in%maj.idx)})/k
    if(!sum(r)){
      new <- matrix(NA,ncol=ncol(minor),nrow=0)
      return(new)
    }
    r <-r/sum(r)
    g <- round(r*G)
    if(!sum(g)){
      new <- matrix(NA,ncol=ncol(minor),nrow=0)
      return(new)
    }
    mg <- g[which(g!=0)]
    mminor <- minor[which(g!=0),-class.column]
    mknn_m <- knn_m[which(g!=0),]
    
    tmp <-cbind(mg,mknn_m)
    sel <- apply(tmp,1,function(x){sample(x[-1],x[1],replace = T)})
    tmp <-cbind(mg,mminor)
    mminor <- matrix(unlist(apply(tmp,1,function(x){rep(x[-1],x[1])})),ncol=ncol(mminor),byrow=T)
    
    new <- mminor+(minor[unlist(sel),-class.column]-mminor)*runif(1)
  }else{
    new <- matrix(NA,ncol=ncol(minor),nrow=0)
    return(new)
  }
  return(cbind(new,1))
}
